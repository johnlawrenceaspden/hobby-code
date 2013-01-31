;; Bringing the session data back onto the server where we can play with it

;;  necessary dependencies
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.7"]]
;; -------------

;; Here's our moral maze app, reduced to its bare essentials, keeping
;; all its data in cookies on the user's browser:

(require 'ring.adapter.jetty
         'ring.middleware.stacktrace
         'ring.middleware.session.cookie
         'ring.middleware.session
         'clojure.pprint)

;; middleware for spying on request maps

(defn html-escape [string]
  (clojure.string/escape string {\< "&lt;", \" "&quot;", \& "&amp;", \> "&gt;"}))

(defn html-pre-escape [string]
  (str "<pre>" (html-escape string) "</pre>"))

(defn format-request [name request kill-keys kill-headers]
  (let [r1 (reduce dissoc request kill-keys)
        r (reduce (fn [h n] (update-in h [:headers] dissoc n)) r1 kill-headers)]
  (with-out-str
    (println "-------------------------------")
    (println name)
    (println "-------------------------------")
    (clojure.pprint/pprint r)
    (println "-------------------------------"))))

(def kill-keys [:body :request-method :character-encoding :remote-addr :server-name :server-port :ssl-client-cert :scheme  :content-type  :content-length])
(def kill-headers ["user-agent" "accept" "accept-encoding" "accept-language" "accept-charset" "connection" "host"])

(defn wrap-spy [handler spyname]
  (fn [request]
    (let [incoming (format-request (str spyname " (request):") request kill-keys kill-headers)]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (format-request (str spyname " (response):") response kill-keys kill-headers)]
          (println outgoing)
          (if (= (type (response :body)) java.lang.String)
                     (update-in response  [:body] (fn[x] (str (html-pre-escape incoming) x  (html-pre-escape outgoing))))
                     response))))))

;; response map makers

(defn status-response [code body]
  {:status code
   :headers {"Content-Type" "text/html"}
   :body body})

(def response (partial status-response 200))

;; functions for outputting strings as html without causing bad things to happen
(defn hppp[x]  (html-pre-escape (with-out-str (binding [clojure.pprint/*print-right-margin* 120] (clojure.pprint/pprint x)))))
(defn hpp[x]  (html-pre-escape (str x)))
(defn hp[x]   (html-escape (str x)))

;; plumbing

(declare handler)

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "handler" )
      (ring.middleware.session/wrap-session {:store (ring.middleware.session.cookie/cookie-store {:key "a 16-byte secret"})})
      ;(wrap-spy "what the server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)))

(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Finally here is the app itself, reduced, I hope, to the absolute essentials

(defn good [request]
  (assoc (response "<h1>good</h1> <a href=\"/\">choose again</a>" )
    :session (update-in (request :session) [:good] (fnil inc 0))))

(defn evil [request]
  (assoc (response "<h1>evil</h1> <a href=\"/\">choose again</a>" )
    :session (update-in (request :session) [:evil] (fnil inc 0))))

(defn home [request]
  (let
      [good   (get-in request [:session :good] 0)
       evil   (get-in request [:session :evil] 0)]
    (response (str "<h1>The Moral Maze</h1>"
                   "Good " good " : Evil " evil 
                   "<p> What do you choose: "
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"))))

(defn handler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I keep adding pages, and I get annoyed with having to copy and
;; paste the handler all the time, so thank you to Nikita Beloglazov
;; who told me how to write this replacement:

(defmacro def-handler [& addresses]
  `(defn handler [~'request]
     (case (~'request :uri)
       ~@(mapcat (fn[x] [(str "/" x) (list x 'request)]) addresses)
       "/" (home ~'request)
       (status-response 404 (str "<h1>404 Not Found: " (:uri ~'request) "</h1>" )))))

(defmacro routefn [& addresses]
  `(fn[~'request]
     (case (~'request :uri)
       ~@(mapcat (fn[x] [(str "/" x) (list x 'request)]) addresses)
       "/" (home ~'request)
       (status-response 404 (str "<h1>404 Not Found: " (:uri ~'request) "</h1>" )))))

(def handler (routefn good evil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; After a bit of worrying, I am very keen on this structure.

;; Consider how easy it is to test: We don't need to involve a real
;; webserver or real state at all, we can just test that the handlers
;; do what they're supposed to do when sent appropriate data:

;; Does a root page exist?
((handler {:uri "/"}) :status) ; -> 200

;; Does looking at the evil page add an evil counter to your session?
((handler {:uri "/evil" :session {:mysesh 'yo}}) :session) ; -> {:evil 1, :mysesh yo}

;; We can define a function which passes a session through a url as if
;; it had been passed in from a browser, processed, and then sent back
;; to be stored in the browser:
(defn sprocess [session uri]
  (let [ns (:session (handler{:uri uri :session session}))]
    (if (nil? ns) session ns)))


;; So here is what happens when a completely unknown browser asks for the home page
(sprocess {} "/home") ;-> {}
;; And if it looks at the evil page:
(sprocess {} "/evil") ;-> {:evil 1}
;; And if it looks at it again:
(sprocess {:evil 1} "/evil") ;{:evil 2} 

;; More concisely, we can change those two looks together
(sprocess (sprocess {} "/evil") "/evil") ;-> {:evil 2}

;; And use the -> macro to make it more readable
(-> {}
    (sprocess "/home")
    (sprocess "/good")
    (sprocess "/evil")
    (sprocess "/good")) ;-> {:evil 1, :good 2}

;; Modifying an accumulator using a sequence of things is a common pattern:
(reduce sprocess {} ["/evil" "/evil" "/good" "/evil" ]) ;-> {:good 1, :evil 3}


;; So our tests for the moral maze website might look something like this:
(use 'clojure.test)

(deftest sitetest
  (testing "page status"
    (is (= (map (fn[x] ((handler {:uri x}) :status)) ["/" "/good" "/evil" ]) '(200 200 200)))
    (is (= (map (fn[x] ((handler {:uri x}) :status)) ["/home" "/favicon.ico" ]) '(404 404))))
  (testing "html"
    (is (re-find #"Good\W*0\W:\WEvil\W0" ((handler {:uri "/"}) :body)))
    (is (re-find #"Good\W*10\W:\WEvil\W0" ((handler {:uri "/" :session {:good 10}}) :body)))
    (is (re-find #"Good\W*10\W:\WEvil\W20" ((handler {:uri "/" :session {:good 10 :evil 20}}) :body))))
  (testing "session"
    (is (= 21 (((handler {:uri "/evil" :session {:good 10 :evil 20}}) :session) :evil)))
    (is (= 10 (((handler {:uri "/evil" :session {:good 10 :evil 20}}) :session) :good)))
    (is (= (reduce sprocess {:userid "fred" :good 2} 
                   ["/evil" "/good" "/" "/home" "/evil" "/favicon.ico" "/evil" "/evil"])
           {:good 3, :evil 4, :userid "fred"}))))




;; They can be hand-run with:
;; (run-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A major advantage of this design is also a major problem with
;; it. All the data is stored in the user's browser in a cookie.

;; This means we can't do statistics on the data, because we don't
;; have it all available at once.

;; But it turns out to be quite easy to bring the data back onto the
;; server where we can see it, because we can use in-memory sessions
;; instead of cookie sessions, and we can also tell ring where to keep
;; them:

(defonce db (atom {}))

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "handler" )
      (ring.middleware.session/wrap-session {:store (ring.middleware.session.memory/memory-store db)})
      ;(wrap-spy "what the server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's make a page where we can see our data:

(defn database [request]
  (response 
   (str "<h1>Database</h1>"
          "<pre>" "(swap! db (fn[x] (merge x " (hppp @db) ")))" "</pre>")))


(def handler (routefn good evil database))


;; Of course, now we've lost some of the advantages of the cookie
;; backed sessions.  A server restart will kill all our data, and we
;; can't any longer run many servers in parallel.

;; I'm hoping that it might be possible to move this data into a
;; database at some point to cure these problems.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; But now that our data *is* on the server, we can do our statistics:

(defn highscores [request]
  (let [score (fn[[k v]] 
                (let [e (v :evil 0)
                      g (v :good 0)
                      r (if (zero? (+ e g)) 1/2 (/ e (+ e g)))]
                  [ r k g e])) 
        hst (sort (map score @db))]
    (response (str
               "<h1>High Score Table</h1>"
             "<table>"
             (str "<tr>""<th>" "User ID"  "<th/>""<th>" "Chose Good" "<th/>""<th>" "Chose Evil" "<th/>" "</tr>")
             (apply str (for [i hst] (str "<tr>""<td>" (i 1)  "<td/>""<td>"  (i 2) "<td/>""<td>" (i 3) "<td/>" "</tr>")))
             "</table>"
             ))))


(def handler (routefn good evil database highscores))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The user ids are a bit distracting, so let's give our users the
;; chance to choose names of their own:

;; For this we'll need the parameter handling middleware
(use 'ring.middleware.params)

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "handler" )
      (ring.middleware.params/wrap-params)
      (ring.middleware.session/wrap-session {:store (ring.middleware.session.memory/memory-store db)})
      ;(wrap-spy "what the server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)))


(defn home [request]
  (let
      [good   (get-in request [:session :good] 0)
       evil   (get-in request [:session :evil] 0)
       name   (get-in request [:session :name] "one who wishes anonymity")]
    (response (str "<h1>The Moral Maze</h1>"
                   "<p>Welcomes: <b>" name "</b>"
                   " (<a href=\"/namechange\">change</a>)"
                   "<p>Good " good " : Evil " evil 
                   "<p> What do you choose: "
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"
                   "<p><hr/><a href=\"/database\">database</a> or <a href=\"/highscores\">high scores</a>"))))


(defn namechange [request]
  (response (str "<form name=\"form\" method=\"post\" action=\"/change-my-name\">"
                 "<input name=\"newname\" value=\"" ((request :session) :name "type name here") "\">")))

(defn change-my-name [request]
  (let [newname ((request :params) "newname")]
    (assoc (response (str "ok " newname "<p><a href=\"/\">back</a>")) :session (assoc (request :session) :name newname))
  ))

(def handler (routefn good evil database highscores namechange change-my-name))

;; Now we can put the user's chosen names in the table instead

(defn highscores [request]
  (let [score (fn[[k v]] 
                (let [e (v :evil 0)
                      g (v :good 0)
                      n (v :name "anon")
                      r (if (zero? (+ e g)) 1/2 (/ e (+ e g)))]
                  [ r n g e k])) 
        hst (sort (map score @db))]
    (response (str
               "<h1>High Score Table</h1>"
             "<table border=1 frame=box rules=rows>"
             (str "<tr>""<th>" "Name"  "<th/>""<th>" "Chose Good" "<th/>""<th>" "Chose Evil" "<th/>" "</tr>")
             (apply str (for [i hst] (str "<tr>""<td>" (hp (i 1))  "<td/>""<td>"  (hp (i 2)) "<td/>""<td>" (hp (i 3)) "<td/>" "</tr>")))
             "</table>"
             ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One remaining problem that we have is that a user's identity is tied to his browser cookie.

;; If someone deletes their cookies, their account can never again be accessed.

;; If they use a different browser, then they will create a second independent account.

;; Well, this feels like a really nasty hack, but it's easy enough to
;; reassociate their browser with a different session:

(defn change-my-identity [request]
  (let [newid ((request :params) "newidentity")]
    (if-let [newsessioncookie (ffirst (filter (fn[[k v]] (=  (v :name) newid)) @db))]
        (assoc (response (str "if you say so...<i>" newid "</i><p><a href=\"/\">home</a>")) 
          :cookies {"ring-session" {:value newsessioncookie}})
        (response "<span style=\"color:red\"><b><i>I think not!</i></b></span>"))))


(defn changeidentity [request]
  (response (str "<form name=\"form\" method=\"post\" action=\"/change-my-identity\">"
                 "If you ain't " ((request :session) :name "dat geezer") " den who <i>are</i> you? :"
                 "<input name=\"newidentity\" value=\"" ((request :session) :name "type name here") "\">")))




(defn home [request]
  (let
      [good   (get-in request [:session :good] 0)
       evil   (get-in request [:session :evil] 0)
       name   (get-in request [:session :name] "one who wishes anonymity")]
    (response (str "<h1>The Moral Maze</h1>"
                   "<p>Welcomes: <b>" name "</b>"
                   " (<a href=\"/namechange\">change</a>)"
                   "<p> (<a href=\"/changeidentity\">not " name  "? log in as someone else.</a>)"
                   "<p>Good " good " : Evil " evil 
                   "<p> What do you choose: "
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"
                   "<p><hr/><a href=\"/database\">database</a> or <a href=\"/highscores\">high scores</a>"))))


(def handler (routefn good evil database highscores namechange change-my-identity change-my-name changeidentity))

;; Finally we need to protect the valuable data in our accounts with passwords

(defn change-my-name [request]
  (let [newname ((request :params) "newname")
        newpassword ((request :params) "password")]
    (if (and newname newpassword)
      (assoc  
          (response (str "ok " newname "<p><a href=\"/\">back</a>")) 
        :session (assoc (request :session) :name newname :password newpassword))
      (response "fail"))))


;; Which can be alternatively phrased:

(defn change-my-name [{{newname "newname" newpassword "password"} :params :as request}]
  (if (and newname newpassword)
    (assoc  
        (response (str "ok " newname "<p><a href=\"/\">back</a>")) 
      :session (assoc (request :session) :name newname :password newpassword))
    (response "fail")))


;; Here's an example of destructuring
((fn [{{n "newname" p "password" :or {n 1 p 2}} :params :as r}  ] (list n p r)) {"password" "doom"}) ;-> (1 2 {"password" "doom"})





(defn namechange [request]
  (response (str "<form name=\"form\" method=\"post\" action=\"/change-my-name\">"
                 "Name: <input name=\"newname\" value=\"" ((request :session) :name "type name here") "\">"
                 "<p>Password: <input name=\"password\" value=\"" ((request :session) :password "f@ilz0r!") "\">"
                 "<input type=\"submit\" value=\"Click!\" />"
                 "</form>")))

(defn changeidentity [request]
  (response (str "<form name=\"form\" method=\"post\" action=\"/change-my-identity\">"
                 "If you ain't " ((request :session) :name "dat geezer") " den who <i>are</i> you? :<p>"
                 "Name    : <input name=\"newidentity\" value=\"" ((request :session) :name "type name here") "\">"
                 "Password: <input name=\"password\" value=\"\">"
                 "<input type=\"submit\" value=\"Click!\" />"
                 "</form>")))


(defn change-my-identity [request]
  (let [newid ((request :params) "newidentity")
        password ((request :params) "password")]
    (if-let [newsessioncookie (ffirst (filter (fn[[k v]] (and (=  (v :name) newid) (= (v :password) password))) @db))]
        (assoc (response (str "if you say so...<i>" newid "</i><p><a href=\"/\">home</a>")) 
          :cookies {"ring-session" {:value newsessioncookie}})
        (response "<span style=\"color:red\"><b><i>I think not!</i></b></span>"))))

(defn passwords [req]
  (response (hppp (for [[ k {n :name p :password}] @db] [n p]))))

(def handler (routefn 
  good evil 
  database passwords highscores 
  namechange change-my-name 
  changeidentity change-my-identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Here's an example database for testing purposes

(swap! db (fn[x] (merge x 
{"83939a50-0073-41b1-8fb0-a85274a67aad" {:good 1},
 "0989d4d5-531d-4e25-bdf7-425a8c62663f" {:evil 1, :name "righteousman", :good 2},
 "099dc04e-8b19-462e-8aff-519b6c5fa50f" {:evil 2, :name "hello world", :good 3, :password "f@ilz0r!"},
 "61252413-28be-4c47-a2f5-37893f19d4b1" {:name "type name here"},
 "7e0a7e86-b00c-4a78-8dd0-2a1ccf627c52" {:name "darkfluffy", :good 2, :password "df"},
 "89be190a-4fb5-4562-aee4-1a65b0d6b415" {:evil 2, :name "fluffy", :good 1, :password "doom"},
 "4c1c2b12-3095-4136-abc1-e9778115cbd0" {:evil 3, :name "atomic man", :good 2},
 "8d2f51c3-bd88-41bf-a8b3-463c9ac96409" {:good 3, :password "pass", :name "mrdoom"},
 "09b5e860-6139-435f-a359-eab110b622c2" {:password "", :name "mrdoom", :evil 5, :good 2}}
)))

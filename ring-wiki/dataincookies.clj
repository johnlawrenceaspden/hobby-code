;;  necessary dependencies
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.6"]]
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
  (str "<pre>" (clojure.string/escape string {\< "&lt;", \> "&gt;"}) "</pre>"))

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
                     (update-in response  [:body] (fn[x] (str (html-escape incoming) x  (html-escape outgoing))))
                     response))))))

;; response map makers

(defn status-response [code body]
  {:status code
   :headers {"Content-Type" "text/html"}
   :body body})

(def response (partial status-response 200))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; After a bit of worrying, I am very keen on this structure.

;; Consider how easy it is to test:


(= ((handler {:uri "/"}) :status) 200)


((handler {:uri "/evil" :session {}}) :session) ;-> {:evil 1}

(defn sprocess [session uri]
  (let [ns (:session (handler{:uri uri :session session}))]
    (if (nil? ns) session ns)))

(sprocess {} "/home") ;-> {}
(sprocess {} "/evil") ;-> {:evil 1}
(sprocess (sprocess {} "/evil") "/evil") ;-> {:evil 2}

(-> {}
    (sprocess "/home")
    (sprocess "/good")
    (sprocess "/evil")
    (sprocess "/good")) ;-> {:evil 1, :good 2}

(reduce sprocess {} ["/evil" "/evil" "/good" "/evil" ]) ;-> {:good 1, :evil 3}

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
           {:good 3, :evil 4, :userid "fred"}))
    ))

;(run-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A major advantage of this design is also a major problem with
;; it. All the data is stored in the user's browser in a cookie.

;; This means we can't do statistics on the data, because we don't
;; have it all available.

;; But it turns out to be quite easy to bring the data back home onto the server:

(defonce db (atom {}))



(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "handler" )
      (ring.middleware.session/wrap-session {:store (ring.middleware.session.memory/memory-store db)})
      ;(wrap-spy "what the server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)))


;; Let's make a page where we can see our data:


(defn database [request]
  (response 
   (str "<h1>Database</h1>" "<ul>"
          (apply str (for [i @db] (str "<li>" (with-out-str (clojure.pprint/pprint i)) "</li>")))
          "</ul>"
          "<h1>Clojure Form</h1>"
          "<pre>" "(swap! db (fn[x] " (with-out-str (clojure.pprint/pprint @db)) "))" "</pre>")))

(defn handler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    "/database" (database request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))


;; Of course, now we've lost some of the advantages of the cookie backed sessions. 
;; A server restart will kill all our data, and we can't any longer run many servers in parallel.

;; I'm hoping that it might be possible to move this data into a
;; database at some point to cure these problems.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now that our data is on the server, we can do our statistics:

(defn highscoretable [request]
  (let [ hst (sort (map (fn[[k v]] [(/ (v :evil) (+ (v :evil) (v :good))) k (v :good) (v :evil)]) @db))]
    (response (str
               "<h1>High Score Table</h1>"
             "<table border=1 frame=box rules=rows>"
             (str "<tr>""<th>" "User ID"  "<th/>""<th>" "Chose Good" "<th/>""<th>" "Chose Evil" "<th/>" "</tr>")
             (apply str (for [i hst] (str "<tr>""<td>" (i 1)  "<td/>""<td>"  (i 2) "<td/>""<td>" (i 3) "<td/>" "</tr>")))
             "</table>"
             ))))



(defn handler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    "/database" (database request)
    "/highscores" (highscoretable request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))

;; Let's give our users the chance to choose names of their own:

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
  (response (str "<form name=\"form\" method=\"post\" action=\"/change-my-name\"><input
name=\"newname\" value=\"" ((request :session) :name "type name here") "\"><br>")))

(defn change-my-name [request]
  (let [newname ((request :params) "newname")]
    (assoc (response (str "ok " newname "<p><a href=\"/\">back</a>")) :session (assoc (request :session) :name newname))
  ))

(defn handler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    "/database" (database request)
    "/highscores" (highscoretable request)
    "/namechange" (namechange request)
    "/change-my-name" (change-my-name request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))

(use 'ring.middleware.params)

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "handler" )
      (ring.middleware.params/wrap-params)
      (ring.middleware.session/wrap-session {:store (ring.middleware.session.memory/memory-store db)})
      ;(wrap-spy "what the server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)))




(defn highscoretable [request]
  (let [ hst (sort (map (fn[[k v]]
                          (let [ e (v :evil 0) g (v :good 0) ]
                          [(/ e (+ e g))
                           (v :name "anon") 
                           g 
                           e]))
                        @db))]
    (response (str
               "<h1>High Score Table</h1>"
             "<table border=1 frame=box rules=rows>"
             (str "<tr>""<th>" "User ID"  "<th/>""<th>" "Chose Good" "<th/>""<th>" "Chose Evil" "<th/>" "</tr>")
             (apply str (for [i hst] (str "<tr>""<td>" (i 1)  "<td/>""<td>"  (i 2) "<td/>""<td>" (i 3) "<td/>" "</tr>")))
             "</table>"
             ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




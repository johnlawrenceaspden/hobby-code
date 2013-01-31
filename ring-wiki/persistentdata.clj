;;  necessary dependencies

;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.7"]]

;; ---------------------------------------------------------------------

;; I'm trying to write a web app using only ring, and no other libraries

;; The point of doing this is to understand how ring and clojure web
;; apps in general work by writing one of my own and dealing with all
;; the details.

;; In the last episode I shaved the yaks of authentication and
;; authorization, and I think I'm now pretty happy with the structure
;; of 'The Moral Maze' from a functionality point of view.

;; The last thing I need to do is to make the data, currently stored
;; in Ring's session mechanism, survive server restarts.

;; ---------------------------------------------------------------------


;; Here is the Moral Maze so far

(require 'ring.adapter.jetty
         'ring.middleware.stacktrace
         'ring.middleware.session.cookie
         'ring.middleware.session
         'ring.middleware.params
         'clojure.pprint)

;; printing data as html
(defn html-escape [string]  (clojure.string/escape string {\< "&lt;", \" "&quot;", \& "&amp;", \> "&gt;"}))
(defn html-pre-escape [string]  (str "<pre>" (html-escape string) "</pre>"))
(defn hppp[x]  (html-pre-escape (with-out-str (binding [clojure.pprint/*print-right-margin* 120] (clojure.pprint/pprint x)))))
(defn hpp[x]  (html-pre-escape (str x)))
(defn hp[x]   (html-escape (str x)))

;; middleware for spying on request maps

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



;; database

(defonce db (atom { "no session" {:name "admin" :password "pa55word" :admin true }}))

;; plumbing

(declare handler)

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "handler" )
      (ring.middleware.params/wrap-params)
      (ring.middleware.session/wrap-session {:store (ring.middleware.session.memory/memory-store db)})
      ;(wrap-spy "what the server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)))

(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))

;; routing 

(defmacro routefn [& addresses]
  `(fn[~'request]
     (case (~'request :uri)
       ~@(mapcat (fn[x] [(str "/" x) (list x 'request)]) addresses)
       "/" (home ~'request)
       (status-response 404 (str "<h1>404 Not Found: " (:uri ~'request) "</h1>" )))))


;; the app

(defn good [request]
  (assoc (response "<h1>good</h1> <a href=\"/\">choose again</a>" )
    :session (update-in (request :session) [:good] (fnil inc 0))))

(defn evil [request]
  (assoc (response "<h1>evil</h1> <a href=\"/\">choose again</a>" )
    :session (update-in (request :session) [:evil] (fnil inc 0))))

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

(defn change-my-name [request]
  (let [newname ((request :params) "newname")
        newpassword ((request :params) "password")]
    (if (and newname newpassword)
      (assoc
          (response (str "ok " newname "<p><a href=\"/\">back</a>"))
        :session (assoc (request :session) :name newname :password newpassword))
      (response "fail"))))

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


(defn passwords [request]
  (if ((request :session) :admin)
    (response (hppp (for [[ k {n :name p :password}] @db] [n p])))
    (response "no way!")))

(defn database [request]
  (if ((request :session) :admin)
    (response
     (str "<h1>Database</h1>"
          "<pre>" "(swap! db (fn[x] (merge x " (hppp @db) ")))" "</pre>"))
    (response "no way!")))


(def handler
  (routefn good evil highscores
           database passwords
           namechange change-my-name
           changeidentity change-my-identity))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's an example database for testing purposes


(swap! db
       (fn[x]
         (merge x
                {"83939a50-0073-41b1-8fb0-a85274a67aad" {:good 1},
                 "0989d4d5-531d-4e25-bdf7-425a8c62663f" {:evil 1, :name "righteousman", :good 2},
                 "099dc04e-8b19-462e-8aff-519b6c5fa50f" {:evil 2, :name "hello world", :good 3, :password "f@ilz0r!"},
                 "61252413-28be-4c47-a2f5-37893f19d4b1" {:name "type name here"},
                 "7e0a7e86-b00c-4a78-8dd0-2a1ccf627c52" {:name "darkfluffy", :good 2, :password "df"},
                 "89be190a-4fb5-4562-aee4-1a65b0d6b415" {:evil 2, :name "fluffy", :good 1, :password "doom"},
                 "4c1c2b12-3095-4136-abc1-e9778115cbd0" {:evil 3, :name "atomic man", :good 2},
                 "ff7a209e-72fb-43f8-90ea-30652f16b4e7" {:good 6, :name "goodguy"},
                 "9f75b42d-17f7-451e-a70e-b36848aeda23" {:name "goodguy", :evil 6}}  )))


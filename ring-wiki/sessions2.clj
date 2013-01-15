;;  necessary dependencies
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.6"]]
;; -------------

;; Here's an app, built in a way which should surprise no-one who's read the previous posts:

(require 'ring.adapter.jetty
         'ring.middleware.stacktrace
         'ring.middleware.session
         'clojure.pprint)

;; Middleware for spying on the doings of other middleware:
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


;; I have taken the liberty of removing some of the less fascinating entries from the request and response maps, for clarity
(def kill-keys [:body :character-encoding :remote-addr :server-name :server-port :ssl-client-cert :scheme  :content-type  :content-length])
(def kill-headers ["user-agent" "accept" "accept-encoding" "accept-language" "accept-charset"])

(defn wrap-spy [handler spyname]
  (fn [request]
    (let [incoming (format-request (str spyname ":\n Incoming Request:") request kill-keys kill-headers)]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (format-request (str spyname ":\n Outgoing Response Map:") response kill-keys kill-headers)]
          (println outgoing)
          (update-in response  [:body] (fn[x] (str (html-escape incoming) x  (html-escape outgoing)))))))))



;; Absolute binding promise to someday get around to writing the app
(declare handler)

;; plumbing
(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the handler sees" )
      (ring.middleware.session/wrap-session )
      (wrap-spy "what the web server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)
      ))

;; The actual application
(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (let [s (request :session)]
           (if (empty? s) 
             (str "<h1>Hello World!</h1>" )
             (str "<h1>Your Session</h1><p>" s "</p>" )))
   :session "I am a session. Fear me."})


;; Start the server if it hasn't already been started
(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Everything so far should be comprehensible.

;; Let's see if we can use the tools we have so far to build a little personality test


(defn status-response [code body]
  {:status code
   :headers {"Content-Type" "text/html"}
   :body body})

(def response (partial status-response 200))


(defn handler [request]
  (case (request :uri)
    "/" (response "<h1>The Moral Maze</h1>What do you choose: <a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?")
    "/good" (response "good")
    "/evil" (response "evil")
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))



;; So far so good. But it would be better if the good and evil pages redirected back to the home page.
(require 'ring.util.response)

(defn handler [request]
  (case (request :uri)
    "/" (response "<h1>The Moral Maze</h1>What do you choose: <a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?")
    "/good" (ring.util.response/redirect "/")
    "/evil" (ring.util.response/redirect "/")
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))

;; Ring has an implementation of 'flash messages', which allows one page to send a message to another.

;; We need to plumb it in:

(require 'ring.middleware.flash)

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the handler sees" )
      (ring.middleware.flash/wrap-flash)
      (wrap-spy "what the flash middleware sees" )      
      (ring.middleware.session/wrap-session )
      (wrap-spy "what the web server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)))


(defn handler [request]
  (case (request :uri)
    "/" (response (str "<h1>The Moral Maze</h1>"
                       (if-let [f (request :flash)]
                         (str "You last chose " (if (= f :evil) "evil" "good") ".<p> What do you choose now:")
                         "What do you choose:")
                       "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"))
    "/good" (assoc (ring.util.response/redirect "/") :flash :good)
    "/evil" (assoc (ring.util.response/redirect "/") :flash :evil)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))


;; So far so good, but what if we want to keep scores for each user?

(defn home [request]
  (let
      [f         (request :flash)
       good   (get-in request [:session :good] 0)
       evil   (get-in request [:session :evil] 0)]
    (response (str "<h1>The Moral Maze</h1>"
                   "Good " good " : Evil " evil "<p>"
                   (if f
                     (str "You last chose " (if (= f :evil) "evil" "good") ".<p> What do you choose now:")
                     "What do you choose: ")
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"))))

(defn good [request]
  (let [ good   (get-in request [:session :good] 0) ]   
    (assoc (ring.util.response/redirect "/") 
      :flash :good 
      :session (assoc (request :session) :good (inc good)))))

(defn evil [request]
  (let [ evil   (get-in request [:session :evil] 0) ]   
    (assoc (ring.util.response/redirect "/") 
      :flash :evil 
      :session (assoc (request :session) :evil (inc evil)))))

(defn handler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))







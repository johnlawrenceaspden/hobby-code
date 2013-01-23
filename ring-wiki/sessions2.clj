;;  necessary dependencies
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.6"]]
;; -------------

;; Here's an app, built in a way which should surprise no-one who's read the previous posts:

(require 'ring.adapter.jetty
         'ring.middleware.stacktrace
         'ring.middleware.session
         'clojure.pprint)

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
    (let [incoming (format-request (str spyname ":\n Incoming Request:") request kill-keys kill-headers)]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (format-request (str spyname ":\n Outgoing Response Map:") response kill-keys kill-headers)]
          (println outgoing)
          (update-in response  [:body] (fn[x] (str (html-escape incoming) x  (html-escape outgoing)))))))))



(declare handler)

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the handler sees" )
      (ring.middleware.session/wrap-session )
      (wrap-spy "what the web server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)
      ))

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (let [s (request :session)]
           (if (empty? s) 
             (str "<h1>Hello World!</h1>" )
             (str "<h1>Your Session</h1><p>" s "</p>" )))
   :session "I am a session. Fear me."})

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
    "/good" (response "<h1>good</h1> <a href=\"/\">choose again</a>" )
    "/evil" (response "<h1>evil</h1> <a href=\"/\">choose again</a>")
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))



;; So far so good. 

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
    "/good" (assoc (response "<h1>good</h1> <a href=\"/\">choose again</a>" ) :flash :good)
    "/evil" (assoc (response "<h1>evil</h1> <a href=\"/\">choose again</a>" ) :flash :evil)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))

;; This works fine in firefox, but the flash messages get lost in chrome because of its constant pestering
;; about favicon.ico. So we'd better make the flash messages persist in that case:

(defn handler [request]
  (case (request :uri)
    "/" (response (str "<h1>The Moral Maze</h1>"
                       (if-let [f (request :flash)]
                         (str "You last chose " (if (= f :evil) "evil" "good") ".<p> What do you choose now:")
                         "What do you choose:")
                       "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"))
    "/good" (assoc (response "<h1>good</h1> <a href=\"/\">choose again</a>" ) :flash :good)
    "/evil" (assoc (response "<h1>evil</h1> <a href=\"/\">choose again</a>" ) :flash :evil)
    "/favicon.ico" {:flash (request :flash)}
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
    (assoc (response "<h1>good</h1> <a href=\"/\">choose again</a>" ) 
      :flash :good 
      :session (assoc (request :session) :good (inc good)))))

(defn evil [request]
  (let [ evil   (get-in request [:session :evil] 0) ]   
    (assoc (response "<h1>evil</h1> <a href=\"/\">choose again</a>" ) 
      :flash :evil 
      :session (assoc (request :session) :evil (inc evil)))))

(defn handler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    "/favicon.ico" {:flash (request :flash)}
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))


;; Let's hide our workings and save the user from potential overclicking injuries

(require 'ring.util.response)

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

;; And then as a final flourish we'll keep total statistics as well

(def goodness (atom 0))
(def evilness (atom 0))

(defn good [request]
  (let [ good   (get-in request [:session :good] 0) ] 
    (swap! goodness inc)
    (assoc (ring.util.response/redirect "/")
      :flash :good 
      :session (assoc (request :session) :good (inc good)))))

(defn evil [request]
  (let [ evil   (get-in request [:session :evil] 0) ]  
    (swap! evilness inc)
    (assoc (ring.util.response/redirect "/")
      :flash :evil 
      :session (assoc (request :session) :evil (inc evil)))))

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
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"
                   "<p> Global Good: " (deref goodness) " Evil: " (deref evilness)))))


;; This all seems to work. But for some reason it makes me deeply uncomfortable.

;; I suppose I shouldn't really be using get requests to modify state,
;; and none of my data is going to survive a server restart, but I
;; don't think that's it.

;; There just seems to be something overcomplicated and fragile about
;; this, even though I don't seem to be able to break it.

;; Can anyone find a way of exposing the problem more clearly, or suggest a better way?








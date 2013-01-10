;;  necessary dependencies
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.6"]]
;; -------------

;; Here's an app, built in a way which should surprise no-one who's read the previous posts:

(require 'ring.adapter.jetty
         'ring.middleware.stacktrace
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


;; I have taken the liberty of removing some of the less interesting entries from the request and response maps, for clarity
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
      (wrap-spy "what the web server sees" )))

;; The actual application
(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<h1>Hello World!</h1>" )})


;; Start the server if it hasn't already been started
(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))

;; Now we'll add the session middleware:

(require 'ring.middleware.session)

;; plumbing
(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the handler sees" )
      (ring.middleware.session/wrap-session)
      (wrap-spy "what the web server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)
      ))


;; If you examine the request as passed to the handler, then you'll
;; see a :session key, whose value currently is {}

;; That seems to be the only difference so far.

;; Now redefine the handler to return a :session key in the response map
(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<h1>Hello World!</h1>" )
   :cookies {"ring-session" {:value ""} "yo" {:value "kill", :max-age 10}}
   ; :cookies {"ring-session" {:value "kill", :max-age 10} "yo" {:value "kill", :max-age 10}}
   ; :session "I am a session. Fear me."
   })




;; This looks to be working in much the same way as




;; The actual application
(defn handler [request]
  (when (not= (request :uri) "/favicon.ico")
    (let [count ((request :session {}) :count 0)]  ;; no, I didn't know you could do this either. neat, isn't it.
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (str "<h1>Hello World(" count ")!</h1>" )
       :session {:count (inc count)}})))

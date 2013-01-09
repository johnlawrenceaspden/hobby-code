;;  necessary dependencies 
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.6"]]
;; -------------

;; Here's an app, built in a way which should surprise no-one who's read the previous posts

(require 'ring.adapter.jetty 
         'ring.middleware.stacktrace 
         'clojure.pprint)

;; Middleware for spying on the doings of other middleware:
(defn html-escape [string] 
  (str "<pre>" (clojure.string/escape string {\< "&lt;", \> "&gt;"}) "</pre>"))

(defn format-request [name request]
  (with-out-str
    (println "-------------------------------")
    (println name)
    (clojure.pprint/pprint request)
    (println "-------------------------------")))

(defn wrap-spy [handler spyname include-body]
  (fn [request]
    (let [incoming (format-request (str spyname ":\n Incoming Request:") request)]
      (println incoming)
      (let [response (handler request)]
        (let [r (if include-body response (assoc response :body "#<?>"))
              outgoing (format-request (str spyname ":\n Outgoing Response Map:") r)]
          (println outgoing)
          (update-in response  [:body] (fn[x] (str (html-escape incoming) x  (html-escape outgoing)))))))))



;; Absolute binding promise to someday get around to writing the app
(declare handler)

;; plumbing
(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the web server sees" false)))  

;; The actual application
(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<h1>Hello World!</h1>" )})

 
;; Start the server if it hasn't already been started
(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Next we'll include We'll include 
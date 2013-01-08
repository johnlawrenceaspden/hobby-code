;;  necessary dependencies 
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.6"]]
;; -------------

;; Behold the mighty app which we have constructed so far:

(require 'ring.adapter.jetty)
(require 'ring.middleware.stacktrace)
(require 'clojure.pprint)

(defn wrap-spy [handler]
  (fn [request]
    (println "-------------------------------")
    (println "Incoming Request:")
    (clojure.pprint/pprint request)
    (let [response (handler request)]
      (println "Outgoing Response Map:")
      (clojure.pprint/pprint response)
      (println "-------------------------------")
      response)))



(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<h1>Hello World!!!!!!!!!!!!!!!</h1>" )})

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy)))

(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))

;; You can access your greeting at:
;; http://localhost:8080 in your favourite browser.

;; Actually, I think it is nice to see the request and response maps as part of the web page
;; So we can modify our spy function

(defn html-escape [string] 
  (str "<pre>" (clojure.string/escape string {\< "&lt;", \> "&gt;"}) "</pre>"))

(defn wrap-spy [handler spyname include-body]
  (fn [request]
    (let [incoming (with-out-str
                     (println "-------------------------------")
                     (println spyname ":\n Incoming Request:")
                     (clojure.pprint/pprint request))]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (with-out-str 
                         (println spyname ":\n Outgoing Response Map:")
                         (clojure.pprint/pprint (if include-body response
                                                  (assoc response :body "#<?>")))
                         (println "-------------------------------"))]
          (println outgoing)
          (update-in response  [:body] (fn[x] (str (html-escape incoming) x  (html-escape outgoing)))))))))))


(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the web server sees" false)
      ))



(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<h1>Hello World!!!!!!!!!!!!!!!</h1>"  )})


;;  necessary dependencies 
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.7"]]
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
          (update-in response  [:body] (fn[x] (str (html-escape incoming) x  (html-escape outgoing)))))))))


(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the web server sees" false)
      ))

;; With our spying middleware, we can investigate the subject of parameters

;; point your browser at http://localhost:8080/?doom=sarnath

;; In the map which is presented to app by jetty and ring, there is the key :query-string
;; Which should have the value "doom=sarnath"

;; We could use that directly

(defn handler [request]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (if-let [s (request :query-string)]
             (let [[a b c] (re-matches #"(.*)=(.*)" (request :query-string))]
                (if (and a b c) (str "<h1>You Have Invoked " b " Upon the City of " c "</h1>")
                    (str "<h1>I do not understand, oh dark master...</h1>" )))
             (str "<h1>Hello World!!!!!!!!!!!!!!!</h1>" ))})

;; But clearly there are issues with that approach.

;; Instead, we can let ring take care of it for us by inserting another piece of middleware:

(require 'ring.middleware.params)

(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (ring.middleware.stacktrace/wrap-stacktrace)
      (ring.middleware.params/wrap-params)
      (wrap-spy "what the web server sees" false)
      ))


;; If you have another look at http://localhost:8080/?Doom=Sarnath

;; You'll see that in between the web server and the handler function, wrap-params has inserted
;; another key, :query-params, with the :query-string already split up into a key-value map

;; So we don't need to parse the query string ourselves any more:

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (if-let [m (request :query-params)]
           (if (empty? m)
             (str "<h1>Hello World!</h1>" )
             (apply str (for [[k v] m] (str "<h1>You Have Invoked " k " Upon the City of " v "</h1>"))))
           (str "<h1>Missing :query-params. Have you included ring.middleware.stacktrace/wrap-stacktrace, oh dark master?</h1>" ))})


             
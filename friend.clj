;;  Friend: Authentication in a Ring App

(def dependencies '[[ring/ring "1.5.0"]])

;; dependency gibberish
(require 'cemerick.pomegranate)
(cemerick.pomegranate/add-dependencies 
 :coordinates dependencies
 :repositories (merge cemerick.pomegranate.aether/maven-central
                      {"clojars" "http://clojars.org/repo"}))

(require 'ring.middleware.stacktrace)
(require 'ring.adapter.jetty)


;; Some infrastructure


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

(declare handler)
(declare app)

(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the web server sees" false)
      ))


(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))


;; Here's the actual app

(defn page1 [request]
    {:status 200
     :headers {"Content-Type""text/html"}
     :body (str  "<h1>Page One</h1>")})

(defn page2 [request]
    {:status 200
     :headers {"Content-Type""text/html"}
     :body (str  "<h1>Page Two</h1>")})


(defn handler [request]
  (case (request :uri)
    "/page1" (page1 request)
    "/page2" (page2 request)
    (page2 request)))
)





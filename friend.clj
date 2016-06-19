;;  Friend: Authentication in a Ring App

(def dependencies '[[ring/ring "1.5.0"]
                    [com.cemerick/friend "0.2.3"]])

;; dependency gibberish
(require 'cemerick.pomegranate)
(cemerick.pomegranate/add-dependencies 
 :coordinates dependencies
 :repositories (merge cemerick.pomegranate.aether/maven-central
                      {"clojars" "http://clojars.org/repo"}))

(require 'ring.middleware.stacktrace)
(require 'ring.middleware.params)
(require 'ring.middleware.keyword-params)
(require 'ring.middleware.nested-params)
(require 'ring.adapter.jetty)
(require '[cemerick.friend :as friend])
(require '[cemerick.friend.credentials :as creds])
(require '[cemerick.friend.workflows :as workflows])


;; Some infrastructure


(defn html-escape [string] 
  (str "<pre>" (clojure.string/escape string {\< "&lt;", \> "&gt;"}) "</pre>"))

(defn wrap-spy [handler spyname include-body]
  (fn [request]
    (let [request (dissoc request :headers :ssl-client-cert :protocol :remote-addr :server-port :content-length :content-type :character-encoding :body :scheme :server-name)
          incoming (with-out-str
                     ;;(println "-------------------------------")
                     (println spyname "")
                     (clojure.pprint/pprint request))]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (with-out-str 
                         (println spyname "")
                         (clojure.pprint/pprint (if include-body response
                                                  (assoc response :body "#<?>")))
                         ;;(println "-------------------------------")
                         )]
          (println outgoing)
          (update-in response  [:body] (fn[x] (str (html-escape incoming) "\n\n" x "\n\n" (html-escape outgoing)))))))))

(declare handler)
(declare app)
(declare users)

(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (ring.middleware.stacktrace/wrap-stacktrace)
      (ring.middleware.params/wrap-params)
      (ring.middleware.keyword-params/wrap-keyword-params)
      (ring.middleware.nested-params/wrap-nested-params)

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

(defn login [request]
    {:status 200
     :headers {"Content-Type""text/html"}
     :body (str  "<h1>Login</h1>")})

(defn not-found [request]
    {:status 404
     :headers {"Content-Type""text/html"}
     :body (str  "<h1>404 ERROR</h1>")})


(defn handler [request]
  (case (request :uri)
    "/page1" (page1 request)
    "/page2" (page2 request)
    "/login" (login request)
    (not-found request)))



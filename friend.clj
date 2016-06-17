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
(require 'ring.adapter.jetty)
(require '[cemerick.friend :as friend])
(require '[cemerick.friend.credentials :as creds])
(require '[cemerick.friend.workflows :as workflows])


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
(declare users)

(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (friend/authenticate {:credential-fn (partial creds/bcrypt-credential-fn users)
                            :workflows [(workflows/interactive-form)]})
      (ring.middleware.stacktrace/wrap-stacktrace)

      ;(wrap-spy "what the web server sees" false)
      ))


(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))


;; Here's the actual app

(def users {"root" {:username "root"
                    :password (creds/hash-bcrypt "admin_password")
                    :roles #{::admin}}
            "jane" {:username "jane"
                    :password (creds/hash-bcrypt "user_password")
                    :roles #{::user}}})



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



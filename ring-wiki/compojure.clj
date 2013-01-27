;;  necessary dependencies
;;  [[org.clojure/clojure "1.4.0"]
;;   [ring "1.1.7"]
;;   [compojure "1.1.4"]]
 
;;------------------------------------------------------

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


;; I have taken the liberty of removing some of the less fascinating entries from the request and response maps, for clarity
(def kill-keys [])
(def kill-headers [])

;(def kill-keys [:body :character-encoding :remote-addr :server-name :server-port :ssl-client-cert :scheme  :content-type  :content-length])
;(def kill-headers ["user-agent" "accept" "accept-encoding" "accept-language" "accept-charset"])


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
      (ring.middleware.stacktrace/wrap-stacktrace) ;; belt
      (wrap-spy "what the handler sees" )
      (wrap-spy "what the web server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace) ;; braces
      ))

;; The actual application
(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<h1>Hello World!</h1>" )})


;; Start the server if it hasn't already been started
(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))



;; Suppose we wanted our site to have two possibilities
;; Let's call them /ping and /pong

(defn handler [request]
  (case (:uri request)
    "/ping" {:status 200
             :headers {"Content-Type" "text/html"}
             :body (str "<h1>ping</h1> <a href=\"/pong\">pong</a>" )}
    "/pong" {:status 200
             :headers {"Content-Type" "text/html"}
             :body (str "<h1>pong</h1> <a href=\"/ping\">ping</a>" )}
    {:status 404 
     :headers {"Content-Type" "text/html"}
     :body (str "<h1>404 Not Found: " (:uri request) "</h1>" )}))


(handler {:uri "/ping"}) ;{:status 200, :headers {"Content-Type" "text/html"}, :body "<h1>ping</h1> <a href=\"/pong\">pong</a>"}
(handler {:uri "/"}) ;-> {:status 404, :headers {"Content-Type" "text/html"}, :body "<h1>404 Not Found: /</h1>"}

;; There's obviously quite a lot of repetition here, so let's see how we might go about abstracting some of it

(defn status-response [code body]
  {:status code
   :headers {"Content-Type" "text/html"}
   :body body})

(def response (partial status-response 200))

(defn handler [request]
  (case (:uri request)
    "/ping" (response (str "<h1>ping</h1> <a href=\"/pong\">pong</a>" ))
    "/pong" (response (str "<h1>pong</h1> <a href=\"/ping\">ping</a>" ))
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))




;; Compojure is a load of predefined macros for doing this sort of thing:






(require 'compojure 'compojure.route)

(compojure.core/defroutes handler
  (compojure.core/ANY "/ping" [] (str "<h1>ping</h1> <a href=\"/pong\">pong</a>" ))
  (compojure.core/ANY "/pong" [] (str "<h1>pong</h1> <a href=\"/ping\">ping</a>" ))
  (compojure.route/not-found "404 Not Found"))

(handler {:uri "/ping"})  ;-> {:status 200, :headers {"Content-Type" "text/html; charset=utf-8"}, :body "<h1>ping</h1> <a href=\"/pong\">pong</a>"}
(handler {:uri "/"})  ; -> {:status 404, :headers {"Content-Type" "text/html; charset=utf-8"}, :body "404 Not Found"} 














;; Let's pull in the compojure library and see what it can do for us:
(require 'compojure :verbose :reload-all)


;;  My sources (http://brehaut.net/blog/2011/ring_introduction) inform me that compojure 


(compojure.core/defroutes ccdr)

(macroexpand '(compojure.core/defroutes ccdr))

(def handler (compojure.core/routes))

(macroexpand '(compojure.core/routes))








;;  A Simple Ring App

(def dependencies '[[ring/ring "1.5.0"]])

;; dependency gibberish
(require 'cemerick.pomegranate)
(cemerick.pomegranate/add-dependencies 
 :coordinates dependencies
 :repositories (merge cemerick.pomegranate.aether/maven-central
                      {"clojars" "http://clojars.org/repo"}))


(require 'ring.middleware.stacktrace
         'ring.middleware.params
         'ring.middleware.keyword-params
         'ring.middleware.nested-params
         'ring.middleware.session
         'ring.adapter.jetty
         'ring.handler.dump)

;; Some infrastructure


(defn html-escape [string] 
  (str "<pre>" (clojure.string/escape string {\< "&lt;", \> "&gt;"}) "</pre>"))

(defn wrap-spy [handler spyname include-body]
  (fn [request]
    (let [stripped-request (dissoc request :headers :ssl-client-cert :protocol :remote-addr :server-port :content-length :content-type :character-encoding :body :scheme :server-name)
          incoming (with-out-str
                     ;;(println "-------------------------------")
                     (println spyname "")
                     (clojure.pprint/pprint stripped-request))]
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

(def app
  (-> #'handler
      (wrap-spy "what the handler sees" true)
      (ring.middleware.stacktrace/wrap-stacktrace)
      (ring.middleware.keyword-params/wrap-keyword-params)
      (ring.middleware.nested-params/wrap-nested-params)
      (ring.middleware.params/wrap-params)
      (ring.middleware.session/wrap-session)
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

(defn not-found [request]
    {:status 404
     :headers {"Content-Type""text/html"}
     :body (str  "<h1>404 ERROR</h1>")})

(defn dump [request]
  (ring.handler.dump/handle-dump request))


(defn handler [request]
  (case (request :uri)
    "/page1" (page1 request)
    "/page2" (page2 request)
    "/login" (login request)
    "/dump"  (dump request)
    (not-found request)))



;; requests can be made programmatically, of course
(app {}) ; {:status 404, :headers {"Content-Type" "text/html"}, :body "<pre>what the web server sees \n{}\n</pre>\n\n<pre>what the handler sees \n{:params {}, :form-params {}, :query-params {}}\n</pre>\n\n<h1>404 ERROR</h1>\n\n<pre>what the handler sees \n{:status 404,\n :headers {\"Content-Type\" \"text/html\"},\n :body \"&lt;h1&gt;404 ERROR&lt;/h1&gt;\"}\n</pre>\n\n<pre>what the web server sees \n{:status 404, :headers {\"Content-Type\" \"text/html\"}, :body \"#&lt;?&gt;\"}\n</pre>"}

(defn summary [response]
  [(:status response) (re-seq #"<h1>.*</h1>" (:body response))])
                                        ;
(summary (app {})) ; [404 ("<h1>404 ERROR</h1>")]
(summary (app {:uri "/"})) ; [404 ("<h1>404 ERROR</h1>")]
(summary (app {:uri "/page1"})) ; [200 ("<h1>Page One</h1>")]
(summary (app {:uri "/page2"})) ; [200 ("<h1>Page Two</h1>")]
(summary (app {:uri "/page3"})) ; [404 ("<h1>404 ERROR</h1>")]
(summary (app {:uri "/login"})) ; [200 ("<h1>Login</h1>")]

;; Although for best effect one should either point a web browser at localhost:8080 or
;; watch -d -n 1 curl -s http://localhost:8080/



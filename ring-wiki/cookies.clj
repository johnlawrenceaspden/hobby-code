;;  necessary dependencies 
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.7"]]
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

;; Next we'll include the cookies middleware

(require 'ring.middleware.cookies)

;; And re-plumb

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the handler sees" true)
      (ring.middleware.cookies/wrap-cookies)
      (wrap-spy "what the web server sees" false)))


;; Now go and look at http://localhost:8080 again.

;; In the map the handler sees, there is a key :cookies, whose value is {}
;; ( If it's not, you might want to clear cookies for localhost from your browser )

;; Let's make our app set a cookie:
(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<h1>Setting Cookie!</h1>" )
   :cookies {"yo" {:value "hi"}} })


;; What happens now is quite complicated. 

;; Our key 
{ :cookies {"yo" {:value "hi"}}}
;; Gets converted by the middleware, and combined with our header, to make
{ :headers {"Set-Cookie" '("yo=hi"), "Content-Type" "text/html"}}
;; in the map given to the jetty adapter

;; If you look at the page with
;; $ curl -sv http://localhost:8080
;; Then you'll see
;; < Set-Cookie: yo=hi
;; as part of the http transaction

;; Now if we look at http://localhost:8080, the response will contain the Set-Cookie header.

;; Most browsers will react to this by including the cookie whenever they contact the site.
;; You can examine cookies from the browser's point of view by 
;; (In Chrome) looking at chrome://chrome/settings/cookies
;; (In Firefox) following some interminable GUI procedure that life is too short to describe. 


;; If you refresh the page yet again, you should now see:
{:headers {"cookie" "yo=hi"}}
;; in the incoming request from the webserver
;; and a new key:
{:cookies {"yo" {:value "hi"}}} 
;; in the map the eventual handler sees (put there by the middleware of course!)



;; We can use this to count how many times a particular browser has been greeted:
(defn seen-before [request]
  (try (Integer/parseInt (((request :cookies) "yo") :value))
       (catch Exception e :never-before)))

(defn handler [request]
  (let [s (seen-before request)]
    (cond
     (= s :never-before) {:status 200
                          :headers {"Content-Type" "text/html"}
                          :body (str "<h1>Hello Stranger!</h1>" )
                          :cookies {"yo" {:value "1"}}}
     (= s 1) {:status 200
                          :headers {"Content-Type" "text/html"}
                          :body (str "<h1>Hello Again!</h1>" )
                          :cookies {"yo" {:value "2"}}}
     :else {:status 200
                          :headers {"Content-Type" "text/html"}
                          :body (str "<h1>Hi, this is visit "s"</h1>" )
                          :cookies {"yo" {:value (str (inc s))}}})))



;; And now, an exercise for the reader!

;; If I look at my site in Firefox, it works as I expected.

;; If I look at it with Chrome, it double counts

;; If I use curl, like so:
;; curl -sv http://localhost:8080 | grep -i hello

;; Then all I ever see is "Hello Stranger"

;; What is going on?




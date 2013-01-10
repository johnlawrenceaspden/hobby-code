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


;; I have taken the liberty of removing some of the less fascinating entries from the request and response maps, for clarity
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


;; Have a look at http://localhost:8080, and while you're there,
;; delete any cookies that your browser is storing for localhost:8080

;; In Chrome, you can right-click on a page, Inspect Element, and then
;; choose Resources/Cookies/localhost to give you a live view of your
;; cookies, which is nice to watch for the following.

;; In Firefox I can't find anything as nice. 

;; With curl of course, you have total control.
;; Real men, those who do not cower behind their mother's apron strings like whining infants
;; may wish to experiment with commands such as:
;; $ curl -sv http://localhost:8080 -b cookies.txt -c cookies.txt && cat cookies.txt


;; Now we'll add the session middleware:

(require 'ring.middleware.session)

;; and re-plumb
(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the handler sees" )
      (ring.middleware.session/wrap-session)
      (wrap-spy "what the web server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)
      ))


;; Have another look at http://localhost:8080, (i.e. refresh the page)


;; If you examine the request as passed to the handler, then you'll
;; see a :session key, whose value currently is {}. That's been
;; inserted by the session middleware.

;; That seems to be the only difference so far.

;; Now redefine the handler to return a :session key/value pair in the response map
(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<h1>Hello World!</h1>" )
   :session "I am a session. Fear me."})


;; And refresh the page again. Only once! And pay careful attention!

;; The incoming maps are exactly the same, of course, but on the way out the middleware
;; notices the :session key, and transforms it into a cookie-setting header

;; Your browser should now have a stored cookie, named ring-session,
;; with a cryptic but hopefully unique random value

;; Again refresh, and notice that this time, the browser presents its cookie, 
;; and the middleware decodes it and puts a :session key in the request.
;; Notice that no new cookie is set the second time.


;; One thing we have control of is the length of time before the session expires
;; Ten seconds is a bit short, but it does allow for some interesting effects:
(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the handler sees" )
      (ring.middleware.session/wrap-session {:cookie-attrs {:max-age 10}})
      (wrap-spy "what the web server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)
      ))


;; Here's an app using sessions to store state in much the same way that we were using cookies earlier:
(defn handler [request]
  (when (not= (request :uri) "/favicon.ico")
    (let [count ((request :session {}) :count 0)]  ;; no, I didn't know you could do this either. neat, isn't it?
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (cond (zero? count) (str "<h1>Hello Stranger!</h1>" )
                   :else (str "<h1>Hello Again (" count ")!</h1>" ))
       :session {:count (inc count)}})))

;; How many times can you ping the virtual goldfish?
;; I got it up to 25 with:
;; watch -d -n 0 curl -sv http://localhost:8080 -b cookies.txt -c cookies.txt
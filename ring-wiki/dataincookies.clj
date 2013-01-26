;;  necessary dependencies
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.6"]]
;; -------------

;; Here's our moral maze app, reduced to its bare essentials, keeping
;; all its data in cookies on the user's browser:

(require 'ring.adapter.jetty
         'ring.middleware.stacktrace
         'ring.middleware.session.cookie
         'ring.middleware.session
         'clojure.pprint)

;; middleware for spying on request maps

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

(def kill-keys [:body :request-method :character-encoding :remote-addr :server-name :server-port :ssl-client-cert :scheme  :content-type  :content-length])
(def kill-headers ["user-agent" "accept" "accept-encoding" "accept-language" "accept-charset" "connection" "host"])

(defn wrap-spy [handler spyname]
  (fn [request]
    (let [incoming (format-request (str spyname " (request):") request kill-keys kill-headers)]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (format-request (str spyname " (response):") response kill-keys kill-headers)]
          (println outgoing)
          (if (= (type (response :body)) java.lang.String)
                     (update-in response  [:body] (fn[x] (str (html-escape incoming) x  (html-escape outgoing))))
                     response))))))

;; response map makers

(defn status-response [code body]
  {:status code
   :headers {"Content-Type" "text/html"}
   :body body})

(def response (partial status-response 200))

;; plumbing

(declare handler)

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "handler" )
      (ring.middleware.session/wrap-session {:store (ring.middleware.session.cookie/cookie-store {:key "a 16-byte secret"})})
      ;(wrap-spy "what the server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)))

(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Finally here is the app itself, reduced, I hope, to the absolute essentials

(defn good [request]
  (assoc (response "<h1>good</h1> <a href=\"/\">choose again</a>" )
    :session (update-in (request :session) [:good] (fnil inc 0))))

(defn evil [request]
  (assoc (response "<h1>evil</h1> <a href=\"/\">choose again</a>" )
    :session (update-in (request :session) [:evil] (fnil inc 0))))

(defn home [request]
  (let
      [good   (get-in request [:session :good] 0)
       evil   (get-in request [:session :evil] 0)]
    (response (str "<h1>The Moral Maze</h1>"
                   "Good " good " : Evil " evil 
                   "<p> What do you choose: "
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"))))

(defn handler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; After a bit of worrying, I am very keen on this structure.

;; Consider how easy it is to test:


(= ((handler {:uri "/"}) :status) 200)


((handler {:uri "/evil" :session {}}) :session) ;-> {:evil 1}

(defn sprocess [session uri]
  (let [ns (:session (handler{:uri uri :session session}))]
    (if (nil? ns) session ns)))

(sprocess {} "/home") ;-> {}
(sprocess {} "/evil") ;-> {:evil 1}
(sprocess (sprocess {} "/evil") "/evil") ;-> {:evil 2}

(-> {}
    (sprocess "/home")
    (sprocess "/good")
    (sprocess "/evil")
    (sprocess "/good")) ;-> {:evil 1, :good 2}

(reduce sprocess {} ["/evil" "/evil" "/good" "/evil" ]) ;-> {:good 1, :evil 3}

(use 'clojure.test)

(deftest sitetest
  (testing "page status"
    (is (= (map (fn[x] ((handler {:uri x}) :status)) ["/" "/good" "/evil" ]) '(200 200 200)))
    (is (= (map (fn[x] ((handler {:uri x}) :status)) ["/home" "/favicon.ico" ]) '(404 404))))
  (testing "html"
    (is (re-find #"Good\W*0\W:\WEvil\W0" ((handler {:uri "/"}) :body)))
    (is (re-find #"Good\W*10\W:\WEvil\W0" ((handler {:uri "/" :session {:good 10}}) :body)))
    (is (re-find #"Good\W*10\W:\WEvil\W20" ((handler {:uri "/" :session {:good 10 :evil 20}}) :body))))
  (testing "session"
    (is (= 21 (((handler {:uri "/evil" :session {:good 10 :evil 20}}) :session) :evil)))
    (is (= 10 (((handler {:uri "/evil" :session {:good 10 :evil 20}}) :session) :good)))
    (is (= (reduce sprocess {:userid "fred" :good 2} 
                   ["/evil" "/good" "/" "/home" "/evil" "/favicon.ico" "/evil" "/evil"])
           {:good 3, :evil 4, :userid "fred"}))
    ))

;(run-tests)










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now, if we get a request from someone we've never seen before, we
;; want to assign her an identity, something like anonymoususer1
;; If we generate random UUIDs then we should be safe from collisions

(defn getanon []
  (str "anonymoususer" (. java.util.UUID randomUUID )))

(defn subhandler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))

(defn handler [request]
  (if-let [userid ((request :session) :_userid)]
    (do 
      (println "request from:" userid)
      (subhandler (assoc request :userid userid)))
    (let [userid (getanon)]
      (println "assigning new:" userid)
      (let [oldsession (request :session)]
        (let [response (subhandler (assoc request :userid userid))]
          (if-let [newsession (response :session)]
            (assoc response :session (assoc newsession :_userid userid))
            (assoc response :session (assoc oldsession :_userid userid))))))))


;; Let's greet our user

(defn home [request]
  (let
      [good   (get-in request [:session :good] 0)
       evil   (get-in request [:session :evil] 0)]
    (response (str "<h1>The Moral Maze</h1>"
                   "hello "(request :userid)"<p>"
                   "Good " good " : Evil " evil "<p>"
                   "What do you choose: "
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"))))


;; Everything should work the same as it did, but a new user is allocated a
;; unique identifier that lasts as long as she doesn't delete her
;; cookies.


;; That means that we can store the results of all her actions on the
;; server's "in-memory database", and stop messing around with the
;; browser's cookie.

(defonce results (atom []))

(defn good [request]
  (swap! results conj [(request :userid), :good])
  (response "<h1>good</h1> <a href=\"/\">choose again</a>" ))

(defn evil [request]
  (swap! results conj [(request :userid), :evil])
  (response "<h1>evil</h1> <a href=\"/\">choose again</a>" ))

(defn home [request]
  (let
      [ r (map second (filter #( = (first %) (request :userid))  @results))
        f (frequencies r)]
    (response (str "<h1>The Moral Maze</h1>"
                   "hello " (request :userid) "<p>"
                   "your choices:" (with-out-str (clojure.pprint/pprint r))
                   "<p>Good " (f :good 0) " : Evil " (f :evil 0) "<p>"
                   "What do you choose: "
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"))))

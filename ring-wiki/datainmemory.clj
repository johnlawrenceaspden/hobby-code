;;  necessary dependencies
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.6"]]
;; -------------

;; I've gone a bit beyond the ring wiki tutorials now. 

;; In case it's not obvious, I have no idea what I'm doing!  I'm
;; trying to work out what sort of structure a real webapp built on
;; ring should have, so I'm trying various approaches.

;; There are all sorts of helpful libraries and frameworks I could be
;; using, but I'm avoiding them deliberately because I think it's
;; important to understand what I'm doing. And if you can't write it,
;; you don't understand it.

;; If anyone's got any good links to 'what a web-app should look like
;; structurally' articles, then please let me know, or leave them in
;; the comments. I can't find anything like that, which is weird for
;; such a widespread problem, and that's one of the reasons why I'm
;; trying to write one. I don't mean particularly in Clojure, I just
;; mean at all.

;; In the previous articles, I made an app which kept all its state in
;; sessions. Those sessions could be put in a cookie backed store, so
;; that all a user's data is stored encrypted on a user's computer,
;; and that might be a nice solution for certain problems.

;; A problem with that is that the only time that the app can see
;; the user's data is when they communicate with the server.

;; So for instance, if the app wanted to know whether people who
;; started off good were likely to turn to evil in later life,
;; information which might well be very useful, it would have a devil
;; of a job to find out.

;; So now I'd like to make a similar app which stores data on the
;; server, and uses the sessions/cookies only to remember the identity
;; of the user of the browser.

;; Eventually I want to move that data into a database on the server,
;; but for now I'm going to keep it in server memory. Of course that
;; means that it will get lost when the server restarts, but one thing
;; at a time.

;; Clojure's memory model is already quite database-like, so
;; presumably it won't be that hard to move the data into a database
;; eventually.

;; To make it easier to think about, I'm going to simplify the problem
;; somewhat. The flash messages are just a distracting detail, and the
;; global counters are redundant if we have all the data in
;; memory. The redirects to the home page make it harder to understand
;; what's going on, so I'm going to remove them an replace them with
;; links back. I'm going to put the session data in a cookie-backed
;; session so that there's no session-related state in the server to
;; worry about.

;; So here's a simplified version of the already very simple character
;; test, which I'll then try to convert to a more centralised design.

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
    (let [incoming (format-request (str spyname ":\n Incoming Request:") request kill-keys kill-headers)]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (format-request (str spyname ":\n Outgoing Response Map:") response kill-keys kill-headers)]
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
      (wrap-spy "what the handler sees" )
      (ring.middleware.session/wrap-session {:store (ring.middleware.session.cookie/cookie-store {:key "a 16-byte secret"})})
      (wrap-spy "what the server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)))

(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Finally here is the app itself, reduced, I hope, to the absolute essentials

(defn good [request]
  (let [ good   (get-in request [:session :good] 0) ] 
    (assoc (response "<h1>good</h1> <a href=\"/\">choose again</a>" )
      :session (assoc (request :session) :good (inc good)))))

(defn evil [request]
  (let [ evil   (get-in request [:session :evil] 0) ]  
    (assoc (response "<h1>evil</h1> <a href=\"/\">choose again</a>" )
      :session (assoc (request :session) :evil (inc evil)))))

(defn home [request]
  (let
      [good   (get-in request [:session :good] 0)
       evil   (get-in request [:session :evil] 0)]
    (response (str "<h1>The Moral Maze</h1>"
                   "Good " good " : Evil " evil "<p>"
                   "What do you choose: "
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"))))

(defn handler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))


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

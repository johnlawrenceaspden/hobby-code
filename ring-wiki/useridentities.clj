;;  necessary dependencies
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.6"]]
;; -------------

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

(defn getanon []
  (str "anonymoususer" (. java.util.UUID randomUUID )))

(declare subhandler)

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


;; The app itself

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


(defn subhandler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))



;; I quite like the structure of our app so far. It feels clean.

;; But from a user's point of view there are a couple of things wrong with it.

;; Firstly, what if they don't like being anonymoususer2c754bf9-ac34-4ceb-9091-b7642670b7de ? 
;; Suppose they'd rather be known as princesspeach?

;; It would be nice if they could change the name of their account.

;; Secondly, what if they log in from a different computer, or a
;; different web browser on the same computer, or delete their
;; cookies?  How do they get back into communication with their old
;; account?

;; These are two different problems, and to solve them we have to
;; dissociate the user's name from the user's identity, and the user's
;; identity from the user's session

;; So firstly let's have a table connecting user identities to names

(defonce users (atom {"administrator" "mighty one"}))

;; And then let us modify our handler so that a new user gets a new name

(defn handler [request]
  (if-let [userid ((request :session) :_userid)]
    (do 
      (println "request from:" userid)
      (subhandler (assoc request :userid userid)))
    (let [userid (getanon)
          name "anonymous user"]
      (println "assigning new:" userid)
      (let [oldsession (request :session)]
        (let [response (subhandler (assoc request :userid userid))]
          (if-let [newsession (response :session)]
            (assoc response :session (assoc newsession :_userid userid))
            (assoc response :session (assoc oldsession :_userid userid))))))))


(defn namechange [userid newname]
  (swap! users (fn [map] (assoc map userid newname))))

(defn changename [request]
  (namechange (request :userid) "princess peach")
  (response "<h1>Peachy got it!</h1> <a href=\"/\">home</a>" ))


(defn subhandler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    "/changename" (changename request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))

(defn home [request]
  (let
      [ r (map second (filter #( = (first %) (request :userid))  @results))
        f (frequencies r)]
    (response (str "<h1>The Moral Maze</h1>"
                   "hello " (@users (request :userid) "?") "<p>"
                   "your choices:" (with-out-str (clojure.pprint/pprint r))
                   "<p>Good " (f :good 0) " : Evil " (f :evil 0) "<p>"
                   "What do you choose: "
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"))))
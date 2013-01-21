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

(defn html-escape [string] (clojure.string/escape string {\< "&lt;", \" "&quot;", \& "&amp;" \> "&gt;"}))

(defn preformatted-escape [string]
  (str "<pre>" (html-escape string) "</pre>"))

(defn format-request [name request kill-keys kill-headers]
  (let [r1 (reduce dissoc request kill-keys)
        r (reduce (fn [h n] (update-in h [:headers] dissoc n)) r1 kill-headers)]
  (with-out-str
    (println "-------------------------------")
    (println name)
    (println "-------------------------------")
    (clojure.pprint/pprint r)
    (println "-------------------------------"))))

(def kill-keys [:body :character-encoding :remote-addr :server-name :server-port :ssl-client-cert :scheme  :content-type  :content-length])
(def kill-headers ["user-agent" "accept" "accept-encoding" "accept-language" "accept-charset" "connection" "host"])

(defn wrap-spy [handler spyname]
  (fn [request]
    (let [incoming (format-request (str spyname ":\n Incoming Request:") request kill-keys kill-headers)]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (format-request (str spyname ":\n Outgoing Response Map:") response kill-keys kill-headers)]
          (println outgoing)
          (if (= (type (response :body)) java.lang.String)
                     (update-in response  [:body] (fn[x] (str (preformatted-escape incoming) x  (preformatted-escape outgoing))))
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

;; So firstly let's have a table connecting user identities to names.
;; We'd like names to be unique, so a two-way dictionary feels like the right structure here.
;; Clojure doesn't have one, so we'll use two dictionaries

(def empty-bidirectional-map [{}{}])

;; I want assoc to change the values associated with keys as it would
;; ordinarily, but never to allow two keys to point to the same value
;; or to change the key associated with an existing value

(defn assoc-unique [[forwardmap backmap] userid name]
  (if (nil? (backmap name))
    (let [oldname (forwardmap userid)]
      [(assoc forwardmap userid name) (assoc (dissoc backmap oldname) name userid)])
    (throw (Exception. "Non-unique username"))))

;; Here's a test map
(def users (atom
                ( -> empty-bidirectional-map
                     (assoc-unique  "identity"  "name")
                     (assoc-unique  "identity"  "newname" )
                     (assoc-unique  "identity2" "name2")
                     (assoc-unique  "identity2" "name"))))


(defn namechange [userid newname]
  (swap! users (fn[map](assoc-unique map userid newname))))

(defn add-user  [userid name]
  (swap! users (fn [map] (assoc-unique map userid name))))

(defn get-name [userid] ((first @users) userid "?"))

;; And then let us modify our handler so that a new user gets a new name as well as an id

(defn handler [request]
  (if-let [userid ((request :session) :_userid)]
    (do 
      (println "request from:" userid)
      (subhandler (assoc request :userid userid)))
    (let [userid (getanon)
          name "anonymous user"]
      (println "assigning new:" userid)
      (add-user userid name)
      (let [oldsession (request :session)]
        (let [response (subhandler (assoc request :userid userid))]
          (if-let [newsession (response :session)]
            (assoc response :session (assoc newsession :_userid userid))
            (assoc response :session (assoc oldsession :_userid userid))))))))


;; We'll need a place for people to enter their new name

(defn changename-form [request]
  (response 
   (str "<form action=\"/changename\" method=\"POST\">
username <input name=username type=\"text\" value=\" " (get-name (request :userid)) "\">
<input type=\"submit\" value=\" change username \">
</form>")))

;; And a url to receive the request

(defn changename [request]
  (if-let [newname ((request :form-params) "username")]
    (try
      (namechange (request :userid) newname)
      (response (str "<h1>" newname "</h1> <a href=\"/\">home</a>" ))
      (catch Exception e
          (response (str "<h1>Illegal Name</h1> <a href=\"/\">home</a>" ))))))


(defn subhandler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    "/changename" (changename request)
    "/changename-form" (changename-form request)
    (status-response 404 (str "<h1>404 Not Found: " (:uri request) "</h1>" ))))

;; We need to use ring's parameter middleware to get our form parameters 
(require 'ring.middleware.params)

(def app
  (-> #'handler
      (ring.middleware.stacktrace/wrap-stacktrace)
      (wrap-spy "what the handler sees" )
      (ring.middleware.params/wrap-params)
      (ring.middleware.session/wrap-session {:store (ring.middleware.session.cookie/cookie-store {:key "a 16-byte secret"})})
      (wrap-spy "what the server sees" )
      (ring.middleware.stacktrace/wrap-stacktrace)))

(defn home [request]
  (let
      [ r (map second (filter #( = (first %) (request :userid))  @results))
        f (frequencies r)]
    (response (str "<h1>The Moral Maze</h1>"
                   "hello " (get-name (request :userid)) " <a href=\"/changename-form\">change name</a> <p>"
                   "your choices:" (with-out-str (clojure.pprint/pprint r))
                   "<p>Good " (f :good 0) " : Evil " (f :evil 0) "<p>"
                   "What do you choose: "
                   "<a href=\"/good\">good</a> or <a href=\"/evil\">evil</a>?"))))


;; This seems functional, but we got a problem:

;; Enter the username "<b>hacker"

;; We want to make sure that our usernames can't do that sort of thing.

(defn changename [request]
  (if-let [submitted-name ((request :form-params) "username")]
    (let [newname (html-escape submitted-name)]
      (try
        (namechange (request :userid) newname)
        (response (str "<h1>" newname "</h1> <a href=\"/\">home</a>" ))
        (catch Exception e
          (response (str "<h1>Illegal Name</h1> <a href=\"/\">home</a>" )))))))

;; At this point, I am beginning to long for a static type
;; system. Strings getting passed around like this is a recipe for
;; disaster.

;; It would be better to have arbitrary-untrusted-strings and
;; html-safe-strings and sql-safe-strings and the like and libraries
;; for converting one to the other and automatic mechanisms for making
;; sure that you don't use the wrong ones in the wrong place.

;; Usually I think that the freedom of dynamic type checking is worth
;; the sacrifice of certainty. But where security issues are
;; concerned, and you have to allow for active subversion attempts of
;; arbitrary sophistication, the problems are just too complex to
;; trust mortals with.
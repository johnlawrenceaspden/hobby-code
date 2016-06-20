;; Friend is not hard, says Adam Bard
;; https://adambard.com/blog/easy-auth-with-friend/


;; dependency gibberish
(def dependencies '[[ring/ring "1.5.0"]
                    [com.cemerick/friend "0.2.3"]
                    [compojure "1.5.1"]])

(require 'cemerick.pomegranate)
(cemerick.pomegranate/add-dependencies 
 :coordinates dependencies
 :repositories (merge cemerick.pomegranate.aether/maven-central
                      {"clojars" "http://clojars.org/repo"}))

(require  '[compojure.core :refer [defroutes GET POST]]
          '[cemerick.friend :as friend]
          '[cemerick.friend.workflows :refer [make-auth]]
          '[ring.middleware.session :refer [wrap-session]]
          '[ring.middleware.params :refer [wrap-params]]
          '[ring.middleware.keyword-params :refer [wrap-keyword-params]]
          '[ring.adapter.jetty :refer [run-jetty]])



;; spymiddleware
(defn html-escape [string] 
  (str "<pre>" (clojure.string/escape string {\< "&lt;", \> "&gt;"}) "</pre>"))

(defn wrap-spy [handler spyname include-body]
  (fn [request]
    (let [stripped-request (dissoc request :ssl-client-cert :protocol :remote-addr :server-port :content-length :content-type :character-encoding :body :scheme :server-name)
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
          (update-in response  [:body] (fn[x] (str (html-escape incoming) "\n\n" x "\n\n" (html-escape outgoing))))
          )))))


;; A friend app

(defroutes app-routes
  (GET "/" [] "Hello everyone <form action=\"logout\" method=\"post\"><button>Submit</button></form>")
  (GET "/authorized" [] (friend/authorize #{::user} "Hello authorized"))
  (friend/logout (POST "/logout" [] "logging out")))

;; if there's a speak=friend parameter, then authorize, otherwise nil
(defn fun-workflow [req]
  (let [speak (get-in req [:params :speak])]
    (when (= speak "friend")
      (make-auth {:identity "friend" :roles #{::user}}))))


(def app
  (-> #'app-routes
      (wrap-spy "what the handler sees" true)
      (friend/authenticate {:workflows [fun-workflow]})
      (wrap-spy "what the friend sees" false)
      (wrap-keyword-params)
      (wrap-params)
      (wrap-session)
      (wrap-spy "what the web server sees" false)
      ))


(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))


;; C-c C-k to load the file in emacs/cider

;; $ curl -sv http://localhost:8080/ -> hello everyone, and a form

;; $ curl -sv http://localhost:8080/authorized -> 302 Found to http://localhost:8080/login

;; $ curl -sv http://localhost:8080/login -> 404 Not Found

;; $ curl -sv http://localhost:8080/?speak=friend -> 303 See Other to / , and sets a cookie

;; $ curl -sv http://localhost:8080/authorized?speak=friend -> 303 See Other to /, also sets ring session cookie


;; I'm not at all sure what's going on here:

;; rm cookie
;; curl -sv http://localhost:8080/authorized -b cookie -c cookie  -> 302 Found to /login
;; curl -sv http://localhost:8080/login?speak=friend -b cookie -c cookie -> 303 See Other to /authorized
;; curl -sv http://localhost:8080/authorized -b cookie -c cookie ->200 Hello authorized

;; It looks as though something remembers where you want to go if you get redirected to /login
;; and so even though that page doesn't exist, the request with speak=friend results in a redirection to /authorized




;; Results with web browser are similar, but attempt to access authorized goes to the login page immediately

;; http://localhost:8080/authorized -> 404 Not Found (problem accessing /login)

;; Any request with ?speak=friend gets redirected to / , whether you're logged in or not
;; http://localhost:8080/authorized?speak=friend -> Hello everyone
;; http://localhost:8080/?speak=friend -> Hello everyone
;; http://localhost:8080/login?speak=friend -> Hello everyone
;; And it sets a ring session cookie, which presumably gets you logged in

;; Once you are logged in
;; http://localhost:8080/authorized -> Hello authorized

(app {:uri "/"})
(app {:uri "/authorized"})
(app {:uri "/authorized" :query-string "speak=friend"})



(app {:headers
      {"accept" "*/*",
       "host" "localhost:8080",
       "user-agent" "curl/7.38.0",
       "cookie" "ring-session=b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c"},
      :uri "/authorized",
      :query-string nil,
      :request-method :get}) ; {:status 200, :headers {"Content-Type" "text/html; charset=utf-8"}, :body "<pre>what the web server sees \n{:headers\n {\"accept\" \"*/*\",\n  \"host\" \"localhost:8080\",\n  \"user-agent\" \"curl/7.38.0\",\n  \"cookie\" \"ring-session=b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c\"},\n :uri \"/authorized\",\n :query-string nil,\n :request-method :get}\n</pre>\n\n<pre>what the friend sees \n{:cookies\n {\"ring-session\" {:value \"b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c\"}},\n :params {},\n :headers\n {\"accept\" \"*/*\",\n  \"host\" \"localhost:8080\",\n  \"user-agent\" \"curl/7.38.0\",\n  \"cookie\" \"ring-session=b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c\"},\n :form-params {},\n :session/key \"b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c\",\n :query-params {},\n :uri \"/authorized\",\n :query-string nil,\n :request-method :get,\n :session\n {:cemerick.friend/identity\n  {:authentications\n   {\"friend\" {:identity \"friend\", :roles #{:user/user}}},\n   :current \"friend\"}}}\n</pre>\n\n<pre>what the handler sees \n{:cookies\n {\"ring-session\" {:value \"b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c\"}},\n :params {},\n :headers\n {\"accept\" \"*/*\",\n  \"host\" \"localhost:8080\",\n  \"user-agent\" \"curl/7.38.0\",\n  \"cookie\" \"ring-session=b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c\"},\n :form-params {},\n :session/key \"b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c\",\n :query-params {},\n :uri \"/authorized\",\n :query-string nil,\n :cemerick.friend/auth-config\n {:default-landing-uri \"/\",\n  :login-uri \"/login\",\n  :credential-fn #function[clojure.core/constantly/fn--4614],\n  :workflows [#function[user/fun-workflow]]},\n :request-method :get,\n :session\n {:cemerick.friend/identity\n  {:authentications\n   {\"friend\" {:identity \"friend\", :roles #{:user/user}}},\n   :current \"friend\"}}}\n</pre>\n\nHello authorized\n\n<pre>what the handler sees \n{:status 200,\n :headers {\"Content-Type\" \"text/html; charset=utf-8\"},\n :body \"Hello authorized\"}\n</pre>\n\n<pre>what the friend sees \n{:status 200,\n :headers {\"Content-Type\" \"text/html; charset=utf-8\"},\n :body \"#&lt;?&gt;\"}\n</pre>\n\n<pre>what the web server sees \n{:status 200,\n :headers {\"Content-Type\" \"text/html; charset=utf-8\"},\n :body \"#&lt;?&gt;\"}\n</pre>"}


(dissoc (app {:headers
              {"accept" "*/*",
               "host" "localhost:8080",
               "user-agent" "curl/7.38.0",
               "cookie" "ring-session=b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c"},
              :uri "/authorized",
              :query-string nil,
              :request-method :get}) :body) ; {:status 200, :headers {"Content-Type" "text/html; charset=utf-8"}}

(dissoc (app {:headers
              {"cookie" "ring-session=b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c"},
              :uri "/authorized",
              :query-string nil,
              :request-method :get}) :body) ; {:status 200, :headers {"Content-Type" "text/html; charset=utf-8"}}

(dissoc (app {:headers
              {"cookie" "ring-session=b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c"},
              :uri "/authorized",
              :request-method :get}) :body) ; {:status 200, :headers {"Content-Type" "text/html; charset=utf-8"}}


(app {:headers
      {"cookie" "ring-session=b6abc512-5f76-4bf9-a257-6ce1b8dc7d0c"},
      :uri "/authorized"})

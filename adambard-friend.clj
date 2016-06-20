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
  (-> app-routes 
      (friend/authenticate {:workflows [fun-workflow]})
      (wrap-keyword-params)
      (wrap-params)
      (wrap-session)
      ))


(defn -main []
  (run-jetty #'app {:port 8080}))

;; C-c C-k to load the file in emacs/cider, (-main) to run it, -main does not return

;; $ curl -sv http://localhost:8080/ -> hello everyone, and a form

;; $ curl -sv http://localhost:8080/authorized -> 302 Found to http://localhost:8080/login

;; $ curl -sv http://localhost:8080/login -> 404 Not Found

;; $ curl -sv http://localhost:8080/?speak=friend -> 303 See Other to / , and sets a cookie

;; $ curl -sv http://localhost:8080/authorized?speak=friend -> 303 See Other to /, also sets ring session cookie





;; Results with web browser are similar, but attempt to access authorized goes to the login page immediately

;; http://localhost:8080/authorized -> 404 Not Found (problem accessing /login)

;; Any request with ?speak=friend gets redirected to / , whether you're logged in or not
;; http://localhost:8080/authorized?speak=friend -> Hello everyone
;; http://localhost:8080/?speak=friend -> Hello everyone
;; http://localhost:8080/login?speak=friend -> Hello everyone
;; And it sets a ring session cookie, which presumably gets you logged in

;; Once you are logged in
;; http://localhost:8080/authorized -> Hello authorized

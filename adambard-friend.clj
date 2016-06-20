(def dependencies '[[ring/ring "1.5.0"]
                    [com.cemerick/friend "0.2.3"]
                    [compojure "1.5.1"]])

;; dependency gibberish
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


(defroutes app-routes
  (GET "/" [] "Hello everyone <form action=\"logout\" method=\"post\"><button>Submit</button></form>")
  (GET "/authorized" [] (friend/authorize #{::user} "Hello authorized"))
  (friend/logout (POST "/logout" [] "logging out")))


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



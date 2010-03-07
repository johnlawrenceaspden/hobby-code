(ns hello)

(use 'compojure)

(def users (ref {"jla" {:username "John Lawrence Aspden" :password "passwd"}}))
(def online-users (ref {}))

(defn pick [m & ks] (map #(m %) ks))

(defn with-head [session title & body]
  (html
   [:head
    [:title title]]
   [:body
    (if-let [user (@online-users (:id session))]
      [:div#user (:username user) (link-to "/logout/" "(Log Out)")]
      [:div#user (link-to "/login/" "(Log In)") ])
    body]))
   

(defn home-page [session]
  (with-head session "Home Page"
    [:h1 "Hello Home Page"]
    [:h3 "and another thing"]))

(defn login-form [session msg]
  (with-head session "Login"
    [:h1 "Login"]
    (when msg [:h4 msg])
    (form-to [:post "/login/"]
      [:table
       [:tr [:td "email"]    [:td (text-field "email")]]
       [:tr [:td "password"] [:td (password-field "psw")]]]
      (submit-button "Login"))))

(defn login-user [session [email password]]
  (println session email password)
  (redirect-to
   (if-let [user (@users email)]
     (if (= password (:password user))
       (dosync
        (alter online-users assoc (:id session) user)
        "/")
       "/login/?msg=Bad username or password")
     "/login/?msg=User does not exist")))

(defn logout-user [session]
  (println session)
  (dosync
   (alter online-users dissoc (session :id)))
  (redirect-to "/"))
   


(defroutes my-app
  (GET "/" (home-page session))
  (GET "/login/*" (login-form session (pick params :msg)))
  (POST "/login/" (login-user session (pick params :email :psw)))
  (GET "/logout/" (logout-user session))
  (ANY "*" (page-not-found)))

(defn hello []
  (println "hello!")
  (run-server {:port 8080} "/*" (servlet (with-session my-app))))

(use 'clojure.test)
(deftest the-test
  (is true))

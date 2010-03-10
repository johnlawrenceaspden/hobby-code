;; at REPL: (do (in-ns 'hello)(load-file "/home/john/hobby-code/require-all-snippet.clj"))
(ns hello)

(use 'compojure)
(use 'clojure.contrib.pprint)
(require 'lessons)

(def users (ref {"jla" {:username "John Lawrence Aspden" :password "passwd"}}))
(def online-users (ref {}))
(def data (ref {"http://localhost:8080/" {:title "Agora" :points 10 :poster "jla"}}))




(defn pick [m & ks] (map #(m %) ks))

(defn with-head [session title & body]
  (html
   [:head
    [:title title]]
   [:body
    [:div#user
    (if-let [user (@online-users (:id session))]
       (list (:username (@users user)) " " (link-to "/logout/" "(Log Out)"))
       (link-to "/login/" "(Log In)")) ]
    body]))
   

(defn format-links [data]
    [:ul (map (fn[x] 
                [:li 
                 (link-to (str "up/" (first x)) "up" ) " "
                 (link-to (str "down/" (first x)) "down" ) " "
                 (link-to (first x) (:title (second x))) "  " 
                 (:points (second x)) " posted by: " 
                 (:username (@users (:poster (second x))))] )
              data)])
  

(defn home-page [session]
  (with-head session "Home Page"
    [:h1 "Agora"]
    (interpose " " (list 
                    (link-to "/" "Refresh")
                    (link-to "/new/" "Add Link")))
    [:h3 "Data"]
    (str @data)
    (format-links @data)
    [:h3 "Users"]
    (str @users)
    [:h3 "Online Users"]
    (str @online-users)))



(defn login-user [session [email password]]
  (println session email password)
  (redirect-to
   (if-let [user (@users email)]
     (if (= password (:password user))
       (dosync
        (alter online-users assoc (:id session) email)
        "/")
       "/login/?msg=Bad username or password")
     "/login/?msg=User does not exist")))

(defn logout-user [session]
  (println session)
  (dosync
   (alter online-users dissoc (session :id)))
  (redirect-to "/"))
   

(defn new-link [session msg]
  (with-head session "New Link"
      [:h1 "New Link"]
      [:h3 "Submit a new link"]
      (when msg [:p {:style "color: red;"} msg])
      (form-to [:post "/new/"]
        [:input {:type "Text" :name "url" :value "http://" :size 48 :title "URL"}]
        [:input {:type "Text" :name "title" :value "" :size 48 :title "Title"}]
        (submit-button "Add link"))
      (link-to "/" "Home")))

(defn login-or-register-form [session msg]
  (with-head session "Login or Register"
    (when msg [:h4 msg])
    [:h1 "Login"]
    (form-to [:post "/login/"]
      [:table
       [:tr [:td "email"]    [:td (text-field "email")]]
       [:tr [:td "password"] [:td (password-field "psw")]]]
      (submit-button "Login"))
    [:h1 "Or Register for an Account"]
    (form-to [:post "/register/"]
      [:table
       (for [field ["Email" "Username" "Password"]]
         [:tr
          [:td field]
          [:td (text-field field)]])]
      (submit-button "Sign Up"))))

(defn add-user [session [email username password]]
  (redirect-to
   (if (@users email)
     "/register/?msg=Email already registered"
     (dosync
       (alter users assoc email {:username username :password password})
       (alter online-users assoc (:id session) email)
       "/"))))
     
(defn add-link [session [title url]]
  (let [post-content {:title title :url url}
        hash-code (. post-content hashCode)]
    (redirect-to
     (dosync 
      (alter data assoc url {:title title :points 1
                             :poster (@online-users (:id session))
                             :content post-content
                             :hash-code hash-code}) "/"))))

(defn vote [session url fn]
  (redirect-to
   (dosync
    (alter data update-in [url :points] fn)
    "/")))

(defmacro guard [session & body]
  `(if (@online-users (:id ~session))
       ~@body
       (redirect-to "/register/")))







(defroutes my-app
  (GET "/"        (home-page session))
  (GET "/new/*"   (guard session (new-link session (:msg params))))
  (POST "/new/"      (add-link session (pick params :title :url)))

  (GET "/login/*" (login-or-register-form session (pick params :msg)))
  (POST "/login/" (login-user session (pick params :email :psw)))

  (GET "/logout/" (logout-user session))

  (GET "/register/*" (login-or-register-form session (:msg params)))
  (POST "/register/" (add-user session (pick params :Email :Username :Password)))

  (GET "/up/*"   (guard session (vote session (:* params) inc)))
  (GET "/down/*" (guard session (vote session (:* params) dec)))

  (GET "/lessonplan" (lessons/lessonplan))

  (ANY "*" (page-not-found)))

(defn hello []
  (println "hello!")
  (run-server {:port 8080} "/*" (servlet (with-session my-app))))

(use 'clojure.test)
(deftest the-test
  (is true))


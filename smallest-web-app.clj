(use 'compojure)

(def posts (ref []))

(defn link [text url] [:a {:href url} text])

(defn layout [title body]
  (html
   [:head [:title title]]
   [:body 
    [:h1 title]
    body
    [:br] (link "home" "/")]))

(defn main-page []
  (layout "main page" 
    [:div
     [:ul (map (fn [x] [:li (str x)]) @posts)]
     (form-to [:post "/add-record"]
       (text-area "text")
       (submit-button "save")
       )
     (form-to [:post "/destroy-database"]
       (submit-button "clear"))]))

;;for post-redirect-get whilst modifying state
(defmacro sync-redirect [& body]
  `(do
     (dosync ~@body)
     (redirect-to "/")))

;;associate actions with pages
(defroutes app
  (GET "/" (main-page))
  (POST "/add-record" (sync-redirect (alter posts conj (:text params))))
  (POST "/destroy-database" (sync-redirect (ref-set posts [])))

  (GET  "*" (layout "uninterpreted GET request"             (str params)))
  (POST "*" (layout "uninterpreted POST request"            (str params)))
  (ANY  "*" (layout "uninterpreted request of unknown type" (str params))))

;;Run the server. If we reload this file into a running image then run-server
;;throws an exception. 
(import '(java.net BindException))
(defn start-server [port]
  (try (run-server {:port port} "/*" (servlet app))
       (println (str "Server started on port " port "."))
       (catch java.net.BindException _
	 (println (str "Failed to start server on port " port ". Already running?")))))

(start-server 8080)




 
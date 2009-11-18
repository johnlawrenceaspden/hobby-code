(use 'compojure)
(use 'clojure.contrib.sql)

;;;;;;;;database

(def db {:classname   "org.hsqldb.jdbcDriver"
         :subprotocol "hsqldb"
         :subname     "file:post-db" })

(defn create-database []
  (create-table :posts
    [:key :int "IDENTITY" "PRIMARY KEY" ]
    [:text :varchar "NOT NULL" ]))

(defn ensure-database-exists []
  (try (with-connection db (create-database))
       (catch Exception e (println "Failed to create database: already exists?"))))

(defn clear-database []
  (with-connection db (drop-table :posts))
  (with-connection db (create-database)))

(defn query-posts [q]
  (with-connection db
    (with-query-results results [q]
    (doall results))))

(defn get-posts []
  (map :text (query-posts "select text from posts")))

(defn insert-post [text]
  (with-connection db
    (insert-values :posts [:text] [text])))

;;;;;; web app


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
     [:ul (map (fn [x] [:li (str x)]) (get-posts))]
     (form-to [:post "/add-record"]
       (text-area "text")
       (submit-button "save")
       )
     (form-to [:post "/destroy-database"]
       (submit-button "clear"))]))

;;associate actions with pages
(defroutes app
  (GET  "/" (main-page))
  (POST "/add-record" (do (insert-post (:text params)) (redirect-to "/")))
  (POST "/destroy-database" (do (clear-database) (redirect-to "/")))
  (ANY  "*" (layout "strange request" (str params))))

(import '(java.net BindException))
(defn start-server [port]
  (try (run-server {:port port} "/*" (servlet app))
       (println (str "Server started on port " port "."))
       (catch java.net.BindException _
	 (println (str "Failed to start server on port " port ". Already running?")))))

(ensure-database-exists)
(start-server 8080)













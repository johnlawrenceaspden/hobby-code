(use 'clojure.contrib.sql)

(def db {:classname "org.hsqldb.jdbcDriver"
         :subprotocol "hsqldb"
         :subname "file:post-db" })

(defn create-posts []
  (create-table :posts
    [:key  :int "IDENTITY" "PRIMARY KEY" ]
    [:text :varchar "NOT NULL" ]))

(defn insert-post [text]
  "add a post and return the key value"
  (with-connection db (transaction
	(insert-values :posts [:text] [text])
	(with-query-results results ["CALL IDENTITY()"]
	  (first (vals (first results)))))))

(defn update-post [key text]
  (with-connection db (update-values :posts ["key=?" key] {:text text})))

(defn delete-post [key]
  (with-connection db
    (delete-rows :posts ["key=?" key])))

(defn query-posts [q]
  "execute sql query q on the database. forces realization of results."
  (with-connection db
    (with-query-results results [q]
    (doall results)))) ;;need doall, or connection is closed before sequence is realized.

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;make a database if it doesn't already exist
(try (with-connection db (create-posts))
     (catch Exception e 
       (println "Failed to create database: already exists?")
       (println (str "Exception was: " e))
       e))

;;some operations
(insert-post "hello")
(insert-post "hello again")
(def killme (insert-post "I will be destroyed"))
(def changeme (insert-post "I will be updated"))

(println (map :text (query-posts "select text from posts")))
(delete-post killme)
(update-post changeme "I have been changed")
(println (map :text (query-posts "select text from posts")))

(query-posts "select * from posts")
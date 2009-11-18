(def db {:classname "org.hsqldb.jdbcDriver"
	 :subprotocol "hsqldb"
	 :subname "file:/home/john/snippet-db"})

(use 'clojure.contrib.sql)
(defn create-snippets []
  (create-table :snippets
		[:id :int "IDENTITY" "PRIMARY KEY"]
		[:body :varchar "NOT NULL"]
		[:created_at :datetime]))

;(with-connection db (create-snippets))

(defn now[] (java.sql.Timestamp. (.getTime (java.util.Date.))))

(defn insert-snippets []
  (let [timestamp (now)]
    (seq
     (insert-values :snippets
		    [:body :created_at]
		    ["(println :boo)" timestamp]
		u    ["(defn foo [] 1)" timestamp]))))

;(with-connection db (insert-snippets))

(defn print-snippets []
  (with-query-results res ["select * from snippets"]
    (println res)))

;(with-connection db (print-snippets))
u
(defn select-snippets []
  (with-connection db
    (with-query-results res ["select * from snippets"] (doall res))))

(defn sql-query [q]
  (with-query-results res q (doall res)))

;(with-connection db (sql-query ["select body from snippets"]))

(defn last-created-id
  "Extract the last created id. Call in transaction with insert."
  []
  (first (vals (first (sql-query ["CALL IDENTITY()"])))))

(defn insert-snippet [body]
  (with-connection db
    (transaction
      (insert-values :snippets
		     [:body :created_at]
		     [body (now)])
      (last-created-id))))

;(insert-snippet "(+ 1 1)")
;(insert-snippet "(ref true)")


;; http://en.wikibooks.org/wiki/Clojure_Programming/Examples/JDBC_Examples#PostgreSQL

;; https://help.ubuntu.com/community/PostgreSQL (Alternative Server Setup)


;; Install postgresql with:
;; $ sudo apt-get install postgresql
;; Also I'm told this gui will help:
;; $ sudo apt-get install pgadmin3
;; Check it's running with:
;; $ sudo service postgresql status

;; su to the postgres account:
;; $ sudo su postgres

;; go and make your own user account a postgres superuser
;; and set a password for it.

;; sudo -u postgres createuser --superuser $USER
;; sudo -u postgres psql
;; \password john
;; \q

;; try it out:
;; $ createdb $USER
;; $ psql

;; Add this to pom.xml to get postgres driver
;; <dependency>
;;   <groupId>postgresql</groupId>
;;   <artifactId>postgresql</artifactId>
;;   <version>9.0-801.jdbc4</version>
;; </dependency>

(use 'clojure.contrib.sql)
 
(let [db-host "localhost"
      db-port 5432
      db-name "john"
      db-user "john"
      db-password "87ystgwgh"]
 
  (def db {:classname "org.postgresql.Driver" ; must be in classpath
           :subprotocol "postgresql"
           :subname (str "//" db-host ":" db-port "/" db-name)
           ; Any additional keys are passed to the driver
           ; as driver-specific properties.
           :user db-user
           :password db-password}))

(defn create-blogs
  "Create a table to store blog entries"
  []
  (clojure.contrib.sql/create-table
   :blogs
   [:id :serial "PRIMARY KEY"]   ;;AUTO_INCREMENT is wrong for postgresql, use SERIAL instead
   [:title "varchar(255)"]
   [:body :text]))

(clojure.contrib.sql/with-connection
   db
   (clojure.contrib.sql/transaction
     (create-blogs)))

(with-connection db 
   (with-query-results rs ["select * from blogs"] 
     ; rs will be a sequence of maps, 
     ; one for each record in the result set. 
     (dorun (map #(println (:id %) (:title %) (:body %)) rs))))

(defn insert-blog-entry
  "Insert data into the table"
  [title,body]
  (clojure.contrib.sql/insert-values
   :blogs
   [:title :body]
   [title body]))


(clojure.contrib.sql/with-connection
   db
   (clojure.contrib.sql/transaction
    (insert-blog-entry "Hello World" "Life is awesome in the lisp world.") ))


(with-connection db
  (clojure.contrib.sql/insert-rows :blogs [-1,"hi" "hello"]))


(with-connection db
  (with-query-results rs ["select * from blogs"]
    (dorun (map #(println (:language :iso_code %)) rs))))

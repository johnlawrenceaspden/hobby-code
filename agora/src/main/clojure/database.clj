;; at REPL: (do (in-ns 'database)(load-file "/home/john/hobby-code/require-all-snippet.clj"))
(ns database)

(defstruct user :password :username :email)
(defstruct datum :link :title)
(defstruct post :datum :user)
(defstruct vote :datum :user)
(def users  (ref '{}))
(def data   (ref '{}))
(def posts  (ref '{}))
(def upvotes   (ref '{}))
(def downvotes (ref '{}))

(defn gethash [obj] (. obj hashCode))  ;(map gethash '(john fred dev8d agora google))

(defn add-thing! [thing table]
  (let [hash (gethash thing)]
    (dosync
     (alter table assoc hash thing))))

(defn add-datum! [datum] (add-thing! datum data))
(defn add-user!  [user]  (add-thing! user users))
(defn add-post!  [post]  (add-thing! post posts))
(defn add-upvote!    [vote]    (add-thing! vote upvotes))
(defn add-downvote!  [vote]    (add-thing! vote downvotes))


(defn upvote! [user datum]
  (add-upvote!   (struct-map vote :datum (gethash datum) :user (gethash user))))
(defn downvote! [user datum]
  (add-downvote! (struct-map vote :datum (gethash datum) :user (gethash user))))

(defn post! [user datum]
  (add-datum! datum)
  (add-post! (struct-map post :datum (gethash datum) :user (gethash user))))


;; Sample data
(def john  (struct-map user :email "agora@aspden.com"   :username "John Lawrence Aspden" :password "p"))
(def fred  (struct-map user :email "fred@fred.com"      :username "Frederick"            :password "f"))
(def dev8d (struct-map user :email "dev8d@dell-mini"    :username "Development User"     :password "d"))

(def agora  (struct-map datum :link "http://localhost:8080/" :title "Agora"))
(def google (struct-map datum :link "http://www.google.com/" :title "Google"))

(doseq [x (list john fred dev8d)] (add-user! x))
(post! john agora)
(post! fred google)
(upvote! dev8d agora)
(upvote! john agora)
(upvote! fred agora)
(downvote! fred google)

;(list @users @data @posts @votes)


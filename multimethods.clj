;; Clojure Multimethods

(defn ticket [age]
  (cond (< age 16) :child
        (> age 64) :ancient
        :else      :adult))

(defrecord person [name age])

(defmulti  print-name (fn [person] (ticket (:age person))))
(defmethod print-name :child   [person] (str "Young "  (:name person)))
(defmethod print-name :adult   [person] (str "Mr "     (:name person)))
(defmethod print-name :ancient [person] (str "Old Mr " (:name person)))

(map print-name (list (person. "Fred" 23)
                      (person. "Jimmy" 12)
                      (person. "Seth" 78)))


;; And an alternative version

(defn pname [person]
     (str
      (case (ticket(:age person))
        :child "Young "
        :adult "Mr "
        :ancient "Old Mr ")
      (:name person)))
       
(map pname (list (person. "Fred" 23)
                 (person. "Jimmy" 12)
                 (person. "Seth" 78)))





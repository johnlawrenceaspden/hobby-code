(defn addresses [person-id]
  (iterate #(do (println %) (inc %)) person-id))

(defn person [id]
  (merge  {:addresses (addresses id)} {:name "john"}))

(def people (map person (range 100)))

(doall (take 5 (:addresses (nth people 10))))
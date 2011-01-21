(ns my-ns)

(defmacro pullall [ns]
  `(do ~@(for [i (map first (ns-publics ns))]
           `(def ~i ~(symbol (str ns "/" i))))))

(pullall clojure.string)

(defn my-reverse
  []
  (reverse "abc"))

(ns my-ns-2)

(defn my-fct-2 []
  (list (my-ns/my-reverse)
        (my-ns/reverse "abc")))

(my-fct-2)

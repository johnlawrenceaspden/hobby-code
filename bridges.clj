;; Solution of the four men on a dark night with a torch and a bridge problem
;; from a blog somewhere

(use 'clojure.contrib.combinatorics)
(use 'clojure.set)
(use 'clojure.contrib.str-utils)
(use 'clojure.contrib.pprint)

(def left #{1 2 5 10})
(def right #{})

(defn forward [left right steps minutes]
  (map #(back
         (difference left %)
         (union right (set %))
         (cons % steps)
         (+ minutes (reduce max %)))
  (combinations left 2)))

(defn back [left right steps minutes]
  (if (empty? left)
    (list steps minutes)
    (map #(forward
           (union left (set %))
           (difference right %)
           (cons % steps)
           (+ minutes (reduce max %)))
         (combinations right 1))))

(pprint (forward left right nil 0))
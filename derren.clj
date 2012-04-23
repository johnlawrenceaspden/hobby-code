(def a '((("alpha"), 1),
         (("alpha", "beta"), 1),
         (("alpha", "beta", "gamma"), 2),
         (("alpha", "beta", "gamma"), 3),
         (("alpha", "beta"), 4),
         (("delta", "epsilon"), 5)))
     

(def ta '("alpha", ("beta", ( 1 , ("gamma", ( 2, 3)), 4))))

(def b '())
(def tb '())

(def c '(("alpha"), 1))
(def tc '("alpha", (1)))

(def d '((("gamma"), 2)
         (("gamma"), 3)
         (("delta"), 4)))

(= (transform b) '())

(map (comp first first) d)
(def p (partition-by (comp first first) c))
(def q (list (first (first (first p))) (map second (first p))))

(defn transform [x]
  (if (empty? x) '()
      (let [p (partition-by (comp first first) c)]
        (map
         (fn [x] (list (first (first x)) (map second x)))
         p))))

(transform d)
        
  
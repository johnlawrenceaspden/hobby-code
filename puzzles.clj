(defn test [x y z]
  (= (+ (* 10 10 x) (* 10 y) z)(+ (* 9 9 z) (* 9 y) x)))

(let [n 10]
  (filter (fn[[x y z]] (test x y z))
          (for [x (range n) 
                y (range n) 
                z (range n)] [x y z])))

(let [dogs 28] (+ (* 4 dogs) (* (- 72 dogs) 2)))


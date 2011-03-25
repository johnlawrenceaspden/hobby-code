;; classic recursion (watch this one, it blows stack)
(defn exp-r [x n]
     (if (zero? n) 1
         (* x (exp-r x (dec n)))))


;; tail recursion 
(defn exp-t [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))


;; functional
(defn exp-f [x n]
  (reduce * (repeat n x)))

;; sneaky (also blows stack, but not so easily)
(defn exp-s [x n]
  (let [square (fn[x] (* x x))]
    (cond (zero? n) 1
          (even? n) (square (exp-s x (/ n 2)))
          :else (* x (exp-s x (dec n))))))

;; library
(require 'clojure.contrib.math)

;;self-consitency test
(let [flist (list exp-r exp-t exp-f clojure.contrib.math/expt exp-s)]
  (every? identity
          (map #(apply = %)
               (partition (count flist)
                          (for [i (list 0 1 10 100 1000 4321)
                                j (list 0 1 10 100 1000 4321)
                                f flist ]
                            (f i j))))))
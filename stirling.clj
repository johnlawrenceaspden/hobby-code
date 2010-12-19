;; Stirling's approximation to the factorial

(defn factorial [n]
  (if (< n 2) 1N (* n (factorial (dec n)))))

(take 10 (map factorial (range))) ;(1N 1N 2N 6N 24N 120N 720N 5040N 40320N 362880N)

(defn abs[x] (if (> x 0) x (- x)))

(defn power [a n]
  (if (= n 0) 1N (* a (power a (dec n)))))

(defn stirling [n]
  (with-precision 20
    (* (bigdec (Math/exp (- n))) (power n n))))


(* (power 200 200) (bigdec (Math/exp (- 200))))

(take 10 (drop 400
               (map (fn [a b] (with-precision 20 (/ a b)))
                    (map factorial (drop 1 (range)))
                    (map stirling (drop 1 (range))))))


(defn divides? [n x]  (zero? (rem x n)))

(divides? 7 49) ; true
(divides? 49 7) ; false

(filter #(divides? 7 %) (range)) ; (0 7 14 21 28 35 42 49 56 63 70 77 84 ...)

(defn divides? [n] (fn[x] (zero? (rem x n))))

(filter (divides? 7) (range)) ; (0 7 14 21 28 35 42 49 56 63 70 77 84 ...)

(filter #(or ((divides? 7) %) ((divides? 3) %)) (range)) ; (0 3 6 7 9 12 14 15 18 21 24 27 28 ...)

(defn fnor [f g] (fn[x] (or (f x) (g x))))

(filter (fnor (divides? 3) (divides? 5)) (range)) ; (0 3 5 6 9 10 12 15 18 20 21 24 25 ...)

(defn fn-combine [f & g]
  (fn [x]
    (apply f (map #(% x) g))))

(defn or-fn [a b] (or a b))

(filter (fn-combine or-fn (divides? 3) (divides? 5)) (range)) ; (0 3 5 6 9 10 12 15 18 20 21 24 25 ...)


(def factorial (reductions * 1N (drop 1 (range))))

(defn factoradic [n] {:pre [(>= n 0)]}
   (loop [a (list 0) n n p 2]
      (if (zero? n) a (recur (conj a (mod n p)) (quot n p) (inc p)))))

(defn nth-permutation [s n] {:pre [(< n (nth factorial (count s)))]}
  (let [d (factoradic n)
        choices (concat (repeat (- (count s) (count d)) 0) d)]
    ((reduce 
        (fn [m i] 
          (let [[left [item & right]] (split-at i (m :rem))]
            (assoc m :rem (concat left right) 
                     :acc (conj (m :acc) item))))
      {:rem s :acc []} choices) :acc)))


(nth-permutation ["red" "blue" "green"] 0) ; ["red" "blue" "green"]
(nth-permutation ["red" "blue" "green"] 1) ; ["red" "green" "blue"]
(nth-permutation ["red" "blue" "green"] 2) ; ["blue" "red" "green"]
(nth-permutation ["red" "blue" "green"] 3) ; ["blue" "green" "red"]
(nth-permutation ["red" "blue" "green"] 4) ; ["green" "red" "blue"]
(nth-permutation ["red" "blue" "green"] 5) ; ["green" "blue" "red"]
(nth-permutation ["red" "blue" "green"] 6) ; failz0r!

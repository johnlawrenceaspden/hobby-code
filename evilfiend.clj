(defn iterator [boxes] (vec boxes))

((iterator [ 8 3 2 6 0 7 1 9 4 5]) 0)

(defn iterations [boxes n] (iterate (iterator boxes) n))

(take 10 (iterations [ 8 3 2 6 0 7 1 9 4 5] 0))

(defn orbit [boxes n]
  (set (take (count boxes) (iterations boxes n))))

;more efficient?
;(defn orbit [boxes n]
;  (set
;   (cons n
;         (take-while #(not (= % n))
;                     (drop 1 (iterations boxes n))))))

(orbit [ 8 3 2 6 0 7 1 9 4 5] 5)

(defn partitions [boxes]
  (set (map #(orbit boxes %) (range (count boxes)))))

(partitions [ 8 3 2 6 0 7 1 9 4 5])
(partitions [ 0 2 1 ])

(defn cycles [boxes]
  (map (fn [[s c]](take c (iterations boxes s)))
       (map (fn[set] [(first set) (count set)]) (partitions boxes))))

(defn signature [boxes]
  (sort (map count (cycles boxes))))

(defn largest-cycle [boxes]
  (apply max (signature boxes)))

(largest-cycle (shuffle (range 10)))

(def perms (iterate shuffle (range 10)))

(def largest-cycles (map largest-cycle (vec perms)))

(frequencies (take 10 largest-cycles))




;; In a randomly chosen permutation, what is the length of the longest cycle?

;; For permutations of (0 1 2), there's 1 perm where the longest cycle is 1
;; 3 where the longest cycle is 2, and 2 where the longest cycle is 3
((0 1 2) 1)
((0 2 1) 2)  
((1 0 2) 2)
((2 1 0) 2)
((1 2 0) 3)     
((2 0 1) 3)


;; Let p be a permutation expressed as a permutation of (range n)
;; (0 2 1) represents (0 1 2) -> (0 2 1)

(defn iterator [p]
  (vec p))

;; What is the orbit of a under the permutation p?
(defn get-cycle [p a]
  (let [it (iterator p)]
    (loop [cycle (list a), g a]
      (let [next (it g)]
        (if (= next a) (reverse cycle)
            (recur (cons next cycle) next ))))))

;; (get-cycle '(0 2 1) 0) (0)
;; (get-cycle '(0 2 1) 1) (0)

;; What is p in cycle notation?
(defn cycles [p]
  (loop [els (set p) cycles '()]
    (if (empty? els) cycles
        (let [cycle (get-cycle p (first els))]
          (recur (apply disj els cycle) (cons cycle cycles))))))

;; (cycles '(0 2 1)) -> ((1 2) (0))

;;What is the signature of p?
(defn signature [boxes]
  (sort (map count (cycles boxes))))

(signature '(0 2 1)) (1 2)
(signature '(0 1 2)) (1 1 1)
(signature '(1 2 0)) (3) 

;;What is the largest cycle of p?
(defn largest-cycle [p]
  (apply max (signature p)))

;;Finally, I get to use it in anger!
(defn factorial [n]
  (if (< n 2) n (* n (factorial (dec n)))))

(defn mapmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn longest-cycle-frequencies [n]
  (mapmap #(/ % (factorial n))
          (frequencies
           (map largest-cycle (clojure.contrib.combinatorics/permutations (range n))))))

;;Here are the frequencies for the first eight permutations
(map longest-cycle-frequencies (range 1 8))

;;({1 1}
;; {1 1/2, 2 1/2}
;; {1 1/6, 2 1/2, 3 1/3}
;; {1 1/24, 2 3/8, 3 1/3, 4 1/4}
;; {1 1/120, 2 5/24, 3 1/3, 4 1/4, 5 1/5}
;; {1 1/720, 2 5/48, 3 5/18, 4 1/4, 5 1/5, 6 1/6}
;; {1 1/5040, 2 11/240, 3 7/36, 4 1/4, 5 1/5, 6 1/6, 7 1/7})

;; It looks as though
;; for cycles of length larger than half the permutation, the chances are 1/n.
;; There's only one perm where all the cycles are length 1, so it gets 1/n!
;; The numbers in between seem mysterious

;; If that assumption is correct, then the airmen's daily chance is
(- 1.0 (reduce + (map #(/ %) (range 51 100)))) ;;0.32182782068980476
;; about 32%, 

  
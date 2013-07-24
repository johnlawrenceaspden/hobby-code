;; Counting Comparisons in  Quicksort

(def comparison-count (atom 0))

(defn swap [v i1 i2] 
   (assoc v i2 (v i1) i1 (v i2)))

(defn ^:dynamic choose-random-element [v start end]
  (+ start (rand-int (inc (- end start)))))

(defn ^:dynamic choose-first-element [v start end]  start)

(defn ^:dynamic choose-last-element  [v start end]  end)

(defn ^:dynamic choose-median-element  [v start end]
  (let [a (v start)
        b (v end)
        middle (+ start (int (/ (- end start) 2)))
        c (v middle)]
    (second (second (sort (list [a start] [b end] [c middle]))))))

(defn ^:dynamic partition-by-pivot [v start end choose-pivot-element]
  (let [ v (swap v start (choose-pivot-element v start end))] ;; swap pivot element with randomly chosen one
    (loop [v v i (inc start) j (inc start)] 
      (if (> j end)
        [(dec i) (swap v start (dec i))]
        (if (< (v start) (v j))
          (recur v i (inc j))
          (recur (swap v i j) (inc i) (inc j)))))))

(defn ^:dynamic qsort
  ([v start end choose-pivot-element]
     (if (> (+ 1 start) end) v  ;; short array, nothing to do
         (do 
           (swap! comparison-count #(+ % (- end start)))
           (let [[index newv] (partition-by-pivot v start end choose-pivot-element) ;; otherwise partition
                 leftsorted (qsort newv start (dec index) choose-pivot-element)] ;; and sort the left half
             (qsort leftsorted (inc index) end choose-pivot-element))))))


(defn make-quicksort [choose-pivot]
  (fn [v] 
    (swap! comparison-count (constantly 0))
    (let [vec (into [] v)]
      (let [sorted (qsort vec 0 (dec (count vec)) choose-pivot)]
        [@comparison-count sorted]))))

(def randomized-quicksort (make-quicksort choose-random-element))

(def vanilla-quicksort (make-quicksort choose-first-element))

(def last-element-quicksort  (make-quicksort choose-last-element))

(def over-complicated-quicksort  (make-quicksort choose-median-element))

(randomized-quicksort [0 4 1 5 2 3 ])   ;-> [8 [0 1 2 3 4 5]]
(vanilla-quicksort [0 4 1 5 2 3 ])      ;-> [12 [0 1 2 3 4 5]]
(last-element-quicksort [0 4 1 5 2 3 ]) ;-> [9 [0 1 2 3 4 5]]
(over-complicated-quicksort [0 4 1 5 2 3 ]) ;-> [9 [0 1 2 3 4 5]]



(let [r (range 10)]
  (for [qs (list randomized-quicksort vanilla-quicksort last-element-quicksort over-complicated-quicksort)
        sq (list (shuffle r) r (reverse r))]
    (let [[count sorted] (qs sq)]
      [count (apply < sorted) sq sorted ])))


(def csjoin clojure.string/join)

(defmacro time-it [qs sq]
  `(str ~'qs "-" (csjoin " " (take 10 ~sq)) "-" (with-out-str (time (~qs ~sq)))))


(print 
 (csjoin "" (let [r (range 100)]
                (for [qs (list randomized-quicksort vanilla-quicksort last-element-quicksort)
                      sq (list (shuffle r) r (reverse r))]
                  (time-it qs sq)))))

(let [r 100]
  (for [qs '(randomized-quicksort vanilla-quicksort last-element-quicksort over-complicated-quicksort)
        sq (list (shuffle (range r)) (range r) (reverse (range r)))]
        (let [qsf (eval qs)]
          (let [[cc result] (qsf sq)]
            [qs cc (take 5 sq) (take 5 result)]))))

(def numbers (map #(Integer/parseInt %) (clojure.string/split-lines (slurp "QuickSort.txt"))))

(for [qs '(randomized-quicksort 
           vanilla-quicksort 
           last-element-quicksort 
           over-complicated-quicksort)]
  (let [qsf (eval qs)]
    (let [[cc result] (qsf numbers)
          check (apply < result)]
      [qs cc check (take 5 numbers) (take 5 result)])))

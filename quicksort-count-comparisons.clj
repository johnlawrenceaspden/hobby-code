;; Counting Comparisons in  Quicksort

(defn swap [v i1 i2] 
   (assoc v i2 (v i1) i1 (v i2)))

(defn ^:dynamic random-pivot-element [v start end]
  (+ start (rand-int (- end start))))

(defn ^:dynamic partition-by-pivot [v start end]
  (let [ v (swap v start (random-pivot-element v start end))] ;; swap first element with randomly chosen one
    (loop [v v i (inc start) j (inc start)] 
      (if (> j end)
        [(dec i) (swap v start (dec i))]
        (if (< (v start) (v j))
          (recur v i (inc j))
          (recur (swap v i j) (inc i) (inc j)))))))

(defn ^:dynamic qsort
  ([v start end]
     (if (> (+ 1 start) end) v  ;; short array, nothing to do
         (let [[index newv] (partition-by-pivot v start end) ;; otherwise partition
               leftsorted (qsort newv start (dec index))] ;; and sort the left half
           (qsort leftsorted (inc index) end)))))


(defn quicksort [v] (let [vec (into [] v)] (qsort vec 0 (dec (count vec)))))

(quicksort (range 10))           ;-> [0 1 2 3 4 5 6 7 8 9]
(quicksort (shuffle (range 10))) ;-> [0 1 2 3 4 5 6 7 8 9]
(quicksort (reverse (range 10))) ;-> [0 1 2 3 4 5 6 7 8 9]

(time (first (quicksort (shuffle (range 2048)))))
(time (first (quicksort (range 2048)))) 
(time (first (quicksort (reverse (range 2048)))))




(time (first (quicksort (range 2048)))) 





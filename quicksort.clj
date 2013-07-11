;; Quicksort

(shuffle (range 20)) ;; [7 13 12 2 15 14 16 1 3 0 9 18 17 10 11 8 4 6 5 19]

(def original [7 13 12 2 15 14 16 1 3 0 9 18 17 10 11 8 4 6 5 19])

;; The fundamental notion in quicksort is partition-by-pivot

;; The first thing we need to do is to choose the 'pivot'.
;; The obvious choice is the first element of our array, 7

;; So our fundamental operation will be to reorder the array so that everything less than 
;; 7 comes before it, and everything greater than 7 comes after it.

;; It's a fair bet that the ability to swap elements will come in handy:
(defn swap [v i1 i2] 
   (assoc v i2 (v i1) i1 (v i2)))

(swap [1 2 3] 0 1)   ;-> [2 1 3]
(swap original 1 19) ;-> [7 19 12 2 15 14 16 1 3 0 9 18 17 10 11 8 4 6 5 13]

;; Here's a function to partition-by-pivot, taking the first element of the array as
;; the pivot.

;; To understand what it's doing, notice that it leaves the pivot
;; alone at the beginning of the array, and maintains this pattern:
;; [ pivot | less than pivot | more than pivot | unscanned ]
;; in the array as it scans through:

;; Once the scan is ended we can put the pivot in its rightful place with a single swap.

(defn ^:dynamic partition-by-pivot [v i j]
  (if (= j (count v))  ;; if the scan is ended
    (swap v 0 (dec i)) ;; put the pivot in the right place
    (if (< (v 0) (v j)) ;; otherwise compare elements
      (partition-by-pivot v i (inc j)) ;; and move the boundaries/swap elements as appropriate
      (partition-by-pivot (swap v i j) (inc i) (inc j))))) ;; to preserve the pattern

;; i and j are the indices of the two moving boundaries in the vector. 
;; We don't need an index for the pivot since it stays at the front.

;; Actions having a way of speaking louder than words, so:  
(require 'clojure.tools.trace)
(clojure.tools.trace/dotrace [partition-by-pivot] (partition-by-pivot original 1 1))

;; TRACE t1304: (partition-by-pivot [7 13 12 2 15 14 16 1 3 0 9 18 17 10 11 8 4 6 5 19] 1 1)
;; TRACE t1305: | (partition-by-pivot [7 13 12 2 15 14 16 1 3 0 9 18 17 10 11 8 4 6 5 19] 1 2)
;; TRACE t1306: | | (partition-by-pivot [7 13 12 2 15 14 16 1 3 0 9 18 17 10 11 8 4 6 5 19] 1 3)
;; TRACE t1307: | | | (partition-by-pivot [7 2 12 13 15 14 16 1 3 0 9 18 17 10 11 8 4 6 5 19] 2 4)
;; TRACE t1308: | | | | (partition-by-pivot [7 2 12 13 15 14 16 1 3 0 9 18 17 10 11 8 4 6 5 19] 2 5)
;; TRACE t1309: | | | | | (partition-by-pivot [7 2 12 13 15 14 16 1 3 0 9 18 17 10 11 8 4 6 5 19] 2 6)
;; TRACE t1310: | | | | | | (partition-by-pivot [7 2 12 13 15 14 16 1 3 0 9 18 17 10 11 8 4 6 5 19] 2 7)
;; TRACE t1311: | | | | | | | (partition-by-pivot [7 2 1 13 15 14 16 12 3 0 9 18 17 10 11 8 4 6 5 19] 3 8)
;; TRACE t1312: | | | | | | | | (partition-by-pivot [7 2 1 3 15 14 16 12 13 0 9 18 17 10 11 8 4 6 5 19] 4 9)
;; TRACE t1313: | | | | | | | | | (partition-by-pivot [7 2 1 3 0 14 16 12 13 15 9 18 17 10 11 8 4 6 5 19] 5 10)
;; TRACE t1314: | | | | | | | | | | (partition-by-pivot [7 2 1 3 0 14 16 12 13 15 9 18 17 10 11 8 4 6 5 19] 5 11)
;; TRACE t1315: | | | | | | | | | | | (partition-by-pivot [7 2 1 3 0 14 16 12 13 15 9 18 17 10 11 8 4 6 5 19] 5 12)
;; TRACE t1316: | | | | | | | | | | | | (partition-by-pivot [7 2 1 3 0 14 16 12 13 15 9 18 17 10 11 8 4 6 5 19] 5 13)
;; TRACE t1317: | | | | | | | | | | | | | (partition-by-pivot [7 2 1 3 0 14 16 12 13 15 9 18 17 10 11 8 4 6 5 19] 5 14)
;; TRACE t1318: | | | | | | | | | | | | | | (partition-by-pivot [7 2 1 3 0 14 16 12 13 15 9 18 17 10 11 8 4 6 5 19] 5 15)
;; TRACE t1319: | | | | | | | | | | | | | | | (partition-by-pivot [7 2 1 3 0 14 16 12 13 15 9 18 17 10 11 8 4 6 5 19] 5 16)
;; TRACE t1320: | | | | | | | | | | | | | | | | (partition-by-pivot [7 2 1 3 0 4 16 12 13 15 9 18 17 10 11 8 14 6 5 19] 6 17)
;; TRACE t1321: | | | | | | | | | | | | | | | | | (partition-by-pivot [7 2 1 3 0 4 6 12 13 15 9 18 17 10 11 8 14 16 5 19] 7 18)
;; TRACE t1322: | | | | | | | | | | | | | | | | | | (partition-by-pivot [7 2 1 3 0 4 6 5 13 15 9 18 17 10 11 8 14 16 12 19] 8 19)
;; TRACE t1323: | | | | | | | | | | | | | | | | | | | (partition-by-pivot [7 2 1 3 0 4 6 5 13 15 9 18 17 10 11 8 14 16 12 19] 8 20)
;; TRACE t1323: | | | | | | | | | | | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1322: | | | | | | | | | | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1321: | | | | | | | | | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1320: | | | | | | | | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1319: | | | | | | | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1318: | | | | | | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1317: | | | | | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1316: | | | | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1315: | | | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1314: | | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1313: | | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1312: | | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1311: | | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1310: | | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1309: | | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1308: | | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1307: | | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1306: | | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1305: | => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; TRACE t1304: => [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]



;; Notice that in the result:
;; [ 5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]
;; (which is much better ordered than the original)
;; the pivot element 7 is in its correct place (element 7) already. 
;; It need never move again.

;; We'd now like to repeat the partition on the two parts of the array on either side of 7 
;; And to do this, we need to know the index where the pivot ended up.

;; Let's firstly modify our function so that it returns the final index of the pivot
(defn ^:dynamic partition-by-pivot [v i j]
  (if (= j (count v))
    [(dec i) (swap v 0 (dec i))] ;; just returning the index as well
    (if (< (v 0) (v j))
      (partition-by-pivot v i (inc j))
      (partition-by-pivot (swap v i j) (inc i) (inc j)))))


(partition-by-pivot original 1 1) ;-> [7 [5 2 1 3 0 4 6 7 13 15 9 18 17 10 11 8 14 16 12 19]]

;; Now we want to partition-by-pivot only the array from 0 to 6, so
;; let's further modify our routine so that it can partition just a
;; part of the array

(defn ^:dynamic partition-by-pivot [v start end]
  (loop [v v i (inc start) j (inc start)] ;; this extra loop just sets up the indices i and j
    (if (> j end)
      [(dec i) (swap v start (dec i))]
      (if (< (v start) (v j))
        (recur v i (inc j))
        (recur (swap v i j) (inc i) (inc j))))))


;; So now we can partition subarrays with respect to their first elements:
(partition-by-pivot [7 13 12 2 15 14 16 1 3 0 19 18 17 10 11 8 4 6 5 9]  0 19)
;-> [7 [5 2 1 3 0 4 6 7 13 15 19 18 17 10 11 8 14 16 12 9]]
(partition-by-pivot ['_ 13 12 2 15 14 16 1 3 0 19 18 17 10 11 8 4 6 5 '_]  1 18)
;-> [12 [_ 5 12 2 1 3 0 10 11 8 4 6 13 15 14 16 19 18 17 _]]
(partition-by-pivot ['_ '_ '_ '_ '_ 14 16 1 3 0 19 '_ '_ '_ '_ '_ '_ '_ '_ '_]  5 10) 
;-> [8 [_ _ _ _ _ 0 1 3 14 16 19 _ _ _ _ _ _ _ _ _]]

;; Once we've got this function, it's obvious how to write quicksort:

;; To quicksort, partition by a pivot and then quicksort both halves
(defn ^:dynamic qsort
  ([v start end]
     (if (> (+ 2 start) end) v  ;; short array, nothing to do
         (let [[index newv] (partition-by-pivot v start end) ;; otherwise partition
               leftsorted (qsort newv start (dec index))] ;; and sort the left half
           (qsort leftsorted (inc index) end))))) ;; and then the right half


;; This makes the interface a bit more user-friendly
(defn quicksort [v] (let [vec (into [] v)] (qsort vec 0 (dec (count vec)))))


(quicksort original) ;-> [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 17 16 18 19]

;; ta-daaah...

(clojure.tools.trace/dotrace [qsort partition-by-pivot] (quicksort original))

;; So how fast is quicksort? 

;; Well...

(time (first (quicksort (shuffle (range 64))))) "Elapsed time: 4.112325 msecs"
(time (first (quicksort (shuffle (range 128))))) "Elapsed time: 9.163108 msecs"
(time (first (quicksort (shuffle (range 256))))) "Elapsed time: 32.184819 msecs"
(time (first (quicksort (shuffle (range 1024))))) "Elapsed time: 135.90394 msecs"
(time (first (quicksort (shuffle (range 2048))))) "Elapsed time: 298.712637 msecs"
(time (first (quicksort (shuffle (range 4096))))) "Elapsed time: 733.392983 msecs"
(time (first (quicksort (shuffle (range 8192))))) "Elapsed time: 1461.434799 msecs"
(time (first (quicksort (shuffle (range 16384))))) "Elapsed time: 3530.573197 msecs"

;; But....

(time (first (quicksort (range 64))))   "Elapsed time: 4.902854 msecs"
(time (first (quicksort (range 128)))) "Elapsed time: 14.469927 msecs"
(time (first (quicksort (range 256)))) "Elapsed time: 36.480616 msecs"
(time (first (quicksort (range 512)))) "Elapsed time: 146.244439 msecs"
(time (first (quicksort (range 1024)))) "Elapsed time: 526.623253 msecs"
(time (first (quicksort (range 2048)))) "Elapsed time: 2131.757814 msecs"
(time (first (quicksort (range 4096)))) ;; stack overflow!!
(time (first (quicksort (range 8192))))

;; And also:

(time (first (quicksort (reverse (range 64))))) "Elapsed time: 15.704443 msecs"
(time (first (quicksort (reverse (range 128))))) "Elapsed time: 57.875302 msecs"
(time (first (quicksort (reverse (range 256))))) "Elapsed time: 170.641883 msecs"
(time (first (quicksort (reverse (range 512))))) "Elapsed time: 658.467259 msecs"
(time (first (quicksort (reverse (range 1024))))) "Elapsed time: 2569.858778 msecs"
(time (first (quicksort (reverse (range 2048))))) "Elapsed time: 11171.441353 msecs"
(time (first (quicksort (reverse (range 4096))))) ;; stack overflow!!
(time (first (quicksort (reverse (range 8192)))))

;; It looks as though it's linear-ish for random input, but quadratic (with a vast recursion depth) for at least some cases.
;; And those cases are easy to find!













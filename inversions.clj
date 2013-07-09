;; Inversions in a permutation

;; Consider ( 2 4 5 1 3 )

;; How many inversions? 

;; 2>1, 4>1 5>1 5>3 4>3 , and that's your lot so there are 5

(require 'clojure.tools.trace)

(def perm '( 2 4 5 1 3 ))

(defn ^:dynamic count-inversions [[head & tail]]
  (if (empty? tail) 0
      (+ (count (filter #(> head %) tail))
         (count-inversions tail))))

(count-inversions perm) ;-> 5

(clojure.tools.trace/dotrace [count-inversions] (count-inversions perm))


;; This should be an order n^2 algorithm, and numerical evidence seems to bear this out:

(time (count-inversions (shuffle (range 64))))   "Elapsed time:   3.547588 msecs"
(time (count-inversions (shuffle (range 128))))  "Elapsed time:   8.207538 msecs"
(time (count-inversions (shuffle (range 256))))  "Elapsed time:  26.555821 msecs"
(time (count-inversions (shuffle (range 512))))  "Elapsed time: 101.162096 msecs"
(time (count-inversions (shuffle (range 1024)))) "Elapsed time: 384.476755 msecs"
(time (count-inversions (shuffle (range 2048)))) "Elapsed time: 1524.966618 msecs"

;; We could also write it by piggybacking on merge sort

(defn merge-sorted [lst1 lst2]
  (cond (empty? lst1) lst2
        (empty? lst2) lst1
        :else (if (< (first lst1) (first lst2))
                (cons (first lst1) (merge-sorted (rest lst1) lst2))
                (cons (first lst2) (merge-sorted lst1 (rest lst2))))))

(merge-sorted '() '()) ; ()
(merge-sorted '(1 3 5) '(2 4 6)) ; (1 2 3 4 5 6)


(defn merge-sort [perm]
  (if (= (count perm) 1) perm
      (let [half (/ (count perm) 2)
            pc (merge-sort (take half perm))
            qc (merge-sort (drop half perm))]
        (merge-sorted pc qc))))

(merge-sort (shuffle (range 15))) ; (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)


;; Which is a much better behaved algorithm

(time (first (merge-sort (shuffle (range 64)))))    "Elapsed time: 2.031963 msecs"
(time (first (merge-sort (shuffle (range 128)))))   "Elapsed time: 4.502598 msecs"
(time (first (merge-sort (shuffle (range 256)))))   "Elapsed time: 10.822116 msecs"
(time (first (merge-sort (shuffle (range 512)))))   "Elapsed time: 21.751998 msecs"
(time (first (merge-sort (shuffle (range 1024)))))  "Elapsed time: 46.627019 msecs"
(time (first (merge-sort (shuffle (range 2048)))))  "Elapsed time: 95.166292 msecs"

;; The trick is to notice that we can split the inversions between 
;; the ones completely within a sublist, and the ones between the two lists

;; for instance if we have the two sorted sublists
;; '(1 3 5) and '(2 4 6)
;; our merge might go:

;; [0 ()] <- (1 3 5) (2 4 6)
;; first element of first list is the low one, so no inversions involving it
;; [0 (1)] <- (3 5) (2 4 6)
;; now 2 is the lowest, so it's in two inversions, with 3 and 5
;; [2 (1 2)] <- (3 5) (4 6)
;; 3 is lowest, so it can't be involved in any (further) inversions
;; [2 (1 2 3)] <- (5) (4 6)
;; Now it's 4, so that's involved in an inversion with 5
;; [3 (1 2 3 4)] <- (5) (6)
;; [3 (1 2 3 4 5)] <- () (6)
;; [3 (1 2 3 4 5 6)] <- () ()]

(defn ^:dynamic merge-sorted-and-count-inversions [cnt acc lst1 lst2]
  (cond (and (empty? lst1) (empty? lst2)) [cnt (reverse acc)]
        (empty? lst1) (merge-sorted-and-count-inversions cnt (cons (first lst2) acc) lst1 (rest lst2))
        (empty? lst2) (merge-sorted-and-count-inversions cnt (cons (first lst1) acc) (rest lst1) lst2)
        :else (if (< (first lst1) (first lst2))
                (merge-sorted-and-count-inversions cnt (cons (first lst1) acc) (rest lst1) lst2)
                (merge-sorted-and-count-inversions (+ cnt (count lst1)) (cons (first lst2) acc) lst1 (rest lst2)))))


(merge-sorted-and-count-inversions 0 '() '(1 3 5) '(2 4 6)) ;-> [3 (1 2 3 4 5 6)]
(merge-sorted-and-count-inversions 0 '() '(4 5 6) '(1 2 3)) ;-> [9 (1 2 3 4 5 6)]
(merge-sorted-and-count-inversions 0 '() '(1 2 3) '(4 5 6)) ;-> [0 (1 2 3 4 5 6)]

;; And we're done. The magic of recursion will count the within-list inversions for us

(defn ^:dynamic merge-sort-and-count-inversions [perm]
  (if (<= (count perm) 1) [0 perm]
      (let [half (/ (count perm) 2)
            [pc pp] (merge-sort-and-count-inversions (take half perm))
            [qc qp] (merge-sort-and-count-inversions (drop half perm))]
        (merge-sorted-and-count-inversions (+ pc qc) '() pp qp))))


(merge-sort-and-count-inversions '()) ;-> [0 ()]
(merge-sort-and-count-inversions '(1 2)) ;-> [0 (1 2)]
(merge-sort-and-count-inversions '(2 1)) ;-> [1 (1 2)]
(merge-sort-and-count-inversions '(1 2 3)) ;-> [0 (1 2 3)]
(merge-sort-and-count-inversions '(1 3 2)) ;-> [1 (1 2 3)]
(merge-sort-and-count-inversions '(3 1 2)) ;-> [2 (1 2 3)]

;; Paranoid Check
(let [perm (shuffle (range 10))
      [cnt _] (merge-sort-and-count-inversions perm)]
  [cnt (count-inversions perm) perm]) 
;-> [24 24 [7 1 9 4 2 6 3 5 0 8]]
;-> [33 33 [9 3 8 4 6 7 2 0 5 1]]
;-> [15 15 [4 2 1 6 5 8 0 3 7 9]]
;-> [19 19 [6 1 2 3 7 9 0 8 5 4]]
       

;; This looks like black magic to me, and I just wrote it. Tracing the function calls makes it more obvious what's going on
(clojure.tools.trace/dotrace 
    [merge-sort-and-count-inversions 
     merge-sorted-and-count-inversions] 
  (merge-sort-and-count-inversions '(4 7 6 1 3 8 5 2)))

;; This has a much more linear feel to it.
(time (merge-sort-and-count-inversions (shuffle (range 64))))   "Elapsed time: 6.909189 msecs"
(time (merge-sort-and-count-inversions (shuffle (range 128))))  "Elapsed time: 13.010243 msecs"
(time (merge-sort-and-count-inversions (shuffle (range 256))))  "Elapsed time: 26.119103 msecs"
(time (merge-sort-and-count-inversions (shuffle (range 512))))  "Elapsed time: 46.33599 msecs"
(time (merge-sort-and-count-inversions (shuffle (range 1024)))) "Elapsed time: 88.383795 msecs"


;; Sadly, it blows stack, so we should use recur:
(defn ^:dynamic merge-sorted-and-count-inversions [cnt acc lst1 lst2]
  (cond (and (empty? lst1) (empty? lst2)) [cnt (reverse acc)]
        (empty? lst1) (recur cnt (cons (first lst2) acc) lst1 (rest lst2))
        (empty? lst2) (recur cnt (cons (first lst1) acc) (rest lst1) lst2)
        :else (if (< (first lst1) (first lst2))
                (recur cnt (cons (first lst1) acc) (rest lst1) lst2)
                (recur (+ cnt (count lst1)) (cons (first lst2) acc) lst1 (rest lst2)))))

(time (first (merge-sort-and-count-inversions (shuffle (range 64)))))    "Elapsed time: 5.566839 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 128)))))   "Elapsed time: 11.337405 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 256)))))   "Elapsed time: 21.258081 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 512)))))   "Elapsed time: 40.224599 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 1024)))))  "Elapsed time: 80.079387 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 2048)))))  "Elapsed time: 167.907037 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 4096)))))  "Elapsed time: 336.719215 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 8192)))))  "Elapsed time: 710.427353 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 16384))))) "Elapsed time: 1567.237007 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 32768))))) "Elapsed time: 3344.416842 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 65536))))) "Elapsed time: 7498.921057 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 131072))))) "Elapsed time: 15613.308648 msecs"

(map / (map #(* % (Math/log %)) (reductions * (repeat 17 2))) '(5 11 21 40 80 167 336 710 1567 3344 7498 15613)) 
;-> (0.2772588722239781 0.504107040407233 0.7921682063542231 1.1090354888959124 1.3862943611198906 1.593823457095922 1.8483924814931874 1.9993879236433352 2.0383038979069736 2.122555959609402 2.082584647059809 2.182128368595557)

(defn ^:dynamic merge-sorted-and-count-inversions [cnt acc lst1 lst2]
  (loop [cnt cnt acc acc lst1 lst1 lst2 lst2 len (count lst1)]
    (cond (and (empty? lst1) (empty? lst2)) [cnt (reverse acc)]
          (empty? lst1) (recur cnt (cons (first lst2) acc) lst1 (rest lst2) len)
          (empty? lst2) (recur cnt (cons (first lst1) acc) (rest lst1) lst2 (dec len))
          :else (if (< (first lst1) (first lst2))
                  (recur cnt (cons (first lst1) acc) (rest lst1) lst2 (dec len))
                  (recur (+ cnt len) (cons (first lst2) acc) lst1 (rest lst2) len)))))


(time (first (merge-sort-and-count-inversions (shuffle (range 64)))))     "Elapsed time: 5.47828 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 128)))))    "Elapsed time: 10.504967 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 256)))))    "Elapsed time: 19.921737 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 512)))))    "Elapsed time: 35.390953 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 1024)))))   "Elapsed time: 77.061476 msecs"  
(time (first (merge-sort-and-count-inversions (shuffle (range 2048)))))   "Elapsed time: 149.909427 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 4096)))))   "Elapsed time: 325.440686 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 8192)))))   "Elapsed time: 682.740661 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 16384)))))  "Elapsed time: 1529.723856 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 32768)))))  "Elapsed time: 3273.988823 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 65536)))))  "Elapsed time: 7402.118523 msecs"
(time (first (merge-sort-and-count-inversions (shuffle (range 131072))))) "Elapsed time: 15262.023195 msecs"


(map / (map #(* % (Math/log %)) (reductions * (repeat 17 2))) '(5 11 19 35 77 149 325 682 1529 3273 7402 15262)) 
;-> (0.2772588722239781 0.504107040407233 0.8755543333388782 1.267469130166757 1.4403058297349514 1.7863658881544897 1.91095345778988 2.0814742313588974 2.088961548737886 2.1685997949690927 2.10959466139617 2.232313603648436)


(def numbers (map #(Integer/parseInt %) (clojure.string/split-lines (slurp "IntegerArray.txt"))))

(first (merge-sort-and-count-inversions numbers))
        
            
        
  
    

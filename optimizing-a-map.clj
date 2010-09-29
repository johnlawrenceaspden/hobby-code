;; When trying to optimize a tight loop in clojure, what is the best strategy?

;; How necessary are the local constants?

;; Various timings (Clojure 1.2)

(do 
  (def somenumbers (for [i (range 1000000)] (rand-int 20))) ;; note sensitive to this (rand-int 20) doubles time compared to (rand-int 10000)
  (def anarray (int-array somenumbers))
  (def another (aclone anarray))
  (def length  (alength anarray)))


;; What does it cost to loop a million times doing nothing?
(let [^ints source anarray
      ^ints destination another
      length (int length)]

  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do 
             (recur (inc i)))))))

;;5.3ms  22 cycles/loop on my 1-processor 4.33GHz system
;;12.5 in clojure-1.3

(time (loop [i 0]
        (if (< i 1000000)
          (do
            (recur (inc i))))))
;;12.5 ms in clojure-1.3

;; What gain do we get from unchecked-inc?
(let [^ints source anarray
      ^ints destination another
      length (int length)]

  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do 
             (recur (unchecked-inc i)))))))

;;4.5ms  19 cycles/loop, so unchecked-inc is worth 3 cycles/loop
;; it doesn't exist in clojure-1.3



;; What about if we want to look at the array, but not do anything?
(let [^ints source anarray
      ^ints destination another
      length (int length)]

  (time
 
     (loop [i 0]
       (if (< i length)
         (do (aget source i)
             (recur (inc i)))))))

;;13ms  56 cycles/loop, so it's 37 cycles for an array access


;; To zero the destination array?
(let [^ints source anarray
      ^ints destination another
      length (int length)
      zero (int 0)]

  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i zero)
             (recur (unchecked-inc i)))))))

;;14ms  60 cycles/loop, so 41 cycles for a write


;; To zero the destination array without using a temporary variable?
(let [^ints source anarray
      ^ints destination another
      length (int length)
      zero (int 0)]

  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (int 0))
             (recur (unchecked-inc i)))))))

;;31 ms 134 cycles/loop, so 74 cycles every loop for the (int 0).

;; To copy the array?
(let [^ints source anarray
      ^ints destination another
      length (int length)
      zero (int 0)]

  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (aget source i))
             (recur (unchecked-inc i)))))))

;;18ms  77 cycles/loop, so 58 cycles for read then write (!= 37+41 ??)


;; To branch on < 10?
(let [^ints source anarray
      ^ints destination another
      length (int length)
      zero (int 0)
      one (int 1)
      ten (int 10)]

  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (let [x (aget source i)] (if (< x ten) zero one)))
             (recur (unchecked-inc i)))))))

;;26ms 112 cycles/loop, so 35 cycles for the branch

;; To dispense with the temporary variables (except the loop counter)?
(let [^ints source anarray
      ^ints destination another
      length (int length)]
  
  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (let [x (aget source i)] (if (< x (int 10)) (int 0) (int 1))))
             (recur (unchecked-inc i)))))))

;;71ms 307 cycles/loop, so 195 cycles for not using temporary vars


;; To branch twice on < 10 5 15?
(let [^ints source anarray
      ^ints destination another
      length (int length)
      zero (int 0)
      one (int 1)
      five (int 5)
      ten (int 10)
      fifteen (int 15)]

  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (let [x (aget source i)] (if (< x ten)
                                                            (if (< x five) zero one)
                                                            (if (< x ten)  five ten))))
             (recur (unchecked-inc i)))))))

;;30ms 129 cycles/loop, so 27 cycles for the second branch

;; To branch twice to different results on < 10 5 15?, apparently hitting some
;; sort of register limit.
(let [^ints source anarray
      ^ints destination another
      length (int length)
      zero (int 0)
      one (int 1)
      five (int 5)
      ten (int 10)
      fifteen (int 15)
      n100 (int 100)
      n200 (int 200)
      n300 (int 300)
      n400 (int 400)]

  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (let [x (aget source i)] (if (< x ten)
                                                            (if (< x five) n100 n200)
                                                            (if (< x fifteen)  n300 n400))))
             (recur (unchecked-inc i)))))))

;;40ms 173 cycles/loop, so 27 cycles for the second branch


;; Why on earth is this the same speed with three branches?

(let [^ints source anarray
      ^ints destination another
      length (int length)]
  (let  [n0 (int 0) n1 (int 1) n2 (int 2) n3 (int 3) n4 (int 4) n6 (int 6)
         n8 (int 8) n9 (int 9) n10 (int 10) n11 (int 11) n12 (int 12) n255 (int 255)]

    (time
 
     (loop [i (int n0)]
       (if (< i length)
         (do (aset destination i (let [x (aget source i)]
                                   (if (< x n8)
                                     (if (< x n3)
                                       (if (< x n2)
                                         (if (< x n1) n255 n1) n3)
                                       (if (< x n6)
                                         (if (< x n4) n4 n3) n2))
                                     (if (< x n11)
                                       (if (< x n10)
                                         (if (< x n9) n3 n3) n2)
                                       (if (< x n12) n1 n0)))))
             (recur (unchecked-inc i)))))))

  )
;;39ms 168 cycles/loop

(let [^ints source anarray
      ^ints destination another
      length (int length)]
  (let  [n0 (int 0) n1 (int 1) n2 (int 2) n3 (int 3) n4 (int 4) n6 (int 6)
         n8 (int 8) n9 (int 9) n10 (int 10) n11 (int 11) n12 (int 12) n255 (int 255)]

    (time
 
     (loop [i n0]
       (if (< i length)
         (do (aset destination i (let [x (aget source i)]
                                         (if (< x (int 8))
                                           (if (< x (int 3))
                                             (if (< x (int 2))
                                               (if (< x (int 1))
                                                 255 1) 3)
                                             (if (< x (int 6))
                                               (if (< x (int 4))
                                                 4 3) 2))
                                           (if (< x (int 11))
                                             (if (< x (int 10))
                                               (if (< x (int 9))
                                                 3 3) 2)
                                             (if (< x (int 12))
                                               1 0)))))
             (recur (unchecked-inc i)))))))

  )
;;133nms 576 cycles/loop


(let [^ints source anarray
      ^ints destination another
      length (int length)]
  (let  [n0 (int 0) n1 (int 1) n2 (int 2) n3 (int 3) n4 (int 4) n6 (int 6)
         n8 (int 8) n9 (int 9) n10 (int 10) n11 (int 11) n12 (int 12) n255 (int 255)]

    (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (let [x (aget source i)]
                                         (if (< x (int 8))
                                           (if (< x (int 3))
                                             (if (< x (int 2))
                                               (if (< x (int 1))
                                                 255 1) 3)
                                             (if (< x (int 6))
                                               (if (< x (int 4))
                                                 4 3) 2))
                                           (if (< x (int 11))
                                             (if (< x (int 10))
                                               (if (< x (int 9))
                                                 3 3) 2)
                                             (if (< x (int 12))
                                               1 0)))))
             (recur (unchecked-inc i)))))))

  )
;;135 ms 576 cycles/loop


(let [^ints source anarray
      ^ints destination another
      length (int length)]
  (let  [n0 (int 0) n1 (int 1) n2 (int 2) n3 (int 3) n4 (int 4) n6 (int 6)
         n8 (int 8) n9 (int 9) n10 (int 10) n11 (int 11) n12 (int 12) n255 (int 255)]

    (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (let [x (aget source i)]
                                         (if (< x (int 8))
                                           (if (< x (int 3))
                                             (if (< x (int 2))
                                               (if (< x (int 1))
                                                 255 1) 3)
                                             (if (< x (int 6))
                                               (if (< x (int 4))
                                                 4 3) 2))
                                           (if (< x (int 11))
                                             (if (< x (int 10))
                                               (if (< x (int 9))
                                                 3 3) 2)
                                             (if (< x (int 12))
                                               1 0)))))
             (recur (unchecked-inc i)))))))

  )
;;135 ms 576 cycles/loop




(let [^ints source anarray
      ^ints destination another
      length (int length)]

  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (let [x (aget source i)] x))
             (recur (unchecked-inc i)))))))

;;18ms 77 cycles/loop




(let [^ints source anarray
      ^ints destination another
      length (int length)]

  (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (int 0))
             (recur (unchecked-inc i)))))))

;;31ms 134 cycles/loop




(let [^ints source anarray
      ^ints destination another
      length (int length)]
  (let [zero (int 0)]
    
    (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i zero)
             (recur (unchecked-inc i))))))))
;;14ms 60 cycles/loop


(let [^ints source anarray
      ^ints destination another
      length (int length)]
  (let [zero (int 0)]
    
    (time
 
     (loop [i (int 0)]
       (if (< i length)
         (do (aset destination i (aget source i))
             (recur (unchecked-inc i))))))))
;;12.5ms 60 cycles/loop








;;cycles/loop
(/ (* 156 1000000 4.333) 10000000) ;;67
(/ (* 300 1000000 1.6) 10000000)   ;;48







;; How does this work in clojure 1.3?

(do 
  (def somenumbers (for [i (range 1000000)] (rand-int 20))) ;; note sensitive to this (rand-int 20) doubles time compared to (rand-int 10000)
  (def anarray (long-array somenumbers))
  (def another (aclone anarray))
  (def length  (long (alength anarray))))


(defn ^:static step ^long [^long x]
  (if (< x 8)
    (if (< x 3)
      (if (< x 2)
        (if (< x 1) 255 1) 3)
      (if (< x 6)
        (if (< x 4) 4 3) 2))
    (if (< x 11)
      (if (< x 10)
        (if (< x 9) 3 3) 2)
      (if (< x 12) 1 0))))


(let [^longs source anarray
      ^longs destination another
      length  (int (alength anarray))]

  (time
     (loop [i 0]
       (if (< i length)
         (do (aset destination i (step (aget source i)))
             (recur (inc i))))))

  )
;;39ms 168 cycles/loop with vast numbers of type hints in 1.2
;;120ms with clojure 1.3, but can remove all type hints from literals


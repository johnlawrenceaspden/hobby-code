(set! *warn-on-reflection* true)
(import '(java.util HashMap))

(defn check-len*
  ([l #^HashMap cache] (check-len* l cache 1))
  ([k #^HashMap cache len]
  (loop [k (long k)
         len (long len)]
    (let [val (. cache get k)
          is-even (even? k)]
      (if val
        (unchecked-dec (unchecked-add len (long val)))
        (if (== k (long 1)) len
              (recur (long (if is-even
                             (bit-shift-right (long k) (long 1))
                             (unchecked-add (long k) 
                                    (unchecked-add (long 1)
                                   (long (bit-shift-right k 1))))))
                     (long (if is-even
                             (unchecked-inc len)
                             (unchecked-add len (long 2)))))))))))

(defn check-max [a b]
    (loop [i (long a)
           max (long 0)
           cache (new HashMap)]
      (if (>= i b) max
        (let [val (check-len* i cache)]
          (. cache put i val)
          (recur (inc i)
                 (long (if (> val max) val max))
                 cache)))))

(defn go-sergio [beg end]
  (time (check-max beg end)))



(defn collatz[n]
  (if (even? n) (/ n 2) (+ 1 (* 3 n))))

(defn collatz-cycle-length [n]
  (inc (count (take-while #(not (= % 1)) (iterate collatz n)))))

(defn collatz-max-cycle-length [begin end]
  (reduce max (map collatz-cycle-length (range begin (inc end)))))

(collatz-max-cycle-length 1 5) ;8
(check-max 1 5) ;8
(take 7 (iterate collatz 1)) ;(1)
(take 7 (iterate collatz 2)) ;(2 1)
(take 7 (iterate collatz 3)) ;(3 10 5 16 8 4 2 1)
(take 7 (iterate collatz 4)) ;(4 2 1)
(take 7 (iterate collatz 5)) ;(5 16 8 4 2 1)

(collatz-max-cycle-length 100 105)
(take 88 (iterate collatz 1000))



(defmacro timevs
  {:added "1.0"}
  [expr1 expr2]
  `(let [start# (. System (nanoTime))
         ret1# ~expr1
         middle# (. System (nanoTime))
         ret2# ~expr2
         end# (. System (nanoTime))
         t1# (- middle# start#)
         t2# (- end# middle#)
         ]
     [ (double (/ t1# t2#)) (/ t1# 1000000.0) (/ t2# 1000000.0) ret1# ret2#]))

(defn vs [start len]
  (let [a start b (+ a (dec len))]
    (timevs
     (collatz-max-cycle-length a b)
     (check-max a b)))) 






;; [0.1351151915318085 12.239615 90.58652 119 119]
;; [0.1936539710297751 15.247956 78.738153 119 119]
;; [0.1531101854948322 12.203717 79.705455 119 119]
;; (vs 1 100)


;; [0.4299057321955695 244.240621 568.125993 179 179]
;; [0.4620589677791847 238.53661 516.247117 179 179]
;; [0.4657987431946749 238.688378 512.428128 179 179]
;; (vs 1 1000)


;; [0.6598711145060823 3336.105112 5055.691996 262 262]
;; [0.6666115900603352 3380.551123 5071.245645 262 262]
;; [0.6774911574604003 3430.79277 5063.966861 262 262]
;; (vs 1 10000)



;; [0.8431933177996713 42501.607346 50405.531506 351 351]
;; [0.8710025927990191 42631.415403 48945.222156 351 351]
;; (vs 1 100000)



;; [1.094176270438306 510246.190334 466329.058781 525 525]
;; (vs 1 1000000)


(defn collatz[n]
  (if (even? n) (/ n 2) (+ 1 (* 3 n))))

(defn collatz-cycle-length-sneaky
  ([n] (collatz-cycle-length-sneaky n 1))
  ([n c] (if (= n 1) c (recur (collatz n) (inc c)))))


(collatz-cycle-length-sneaky 10 1)
(collatz-cycle-length-sneaky 5  2)
(collatz-cycle-length-sneaky 16 3)
(collatz-cycle-length-sneaky 8  4)
(collatz-cycle-length-sneaky 4  5)
(collatz-cycle-length-sneaky 2  6)
(collatz-cycle-length-sneaky 1  7)


(map collatz-cycle-length-sneaky (range 1 11))
(map collatz-cycle-length (range 1 11))

(defn collatz-max-cycle-length [begin end]
  (reduce max (map collatz-cycle-length (range begin (inc end)))))

(defn collatz-max-cycle-length-sneaky [begin end]
  (reduce max (map collatz-cycle-length-sneaky (range begin (inc end)))))

(defn collatz-max-cycle-length-ultra-sneaky [begin end]
  (reduce max (map collatz-cycle-length-ultra-sneaky (range begin (inc end)))))


(defn collatz-cycle-length-ultra-sneaky
  ([n] (collatz-cycle-length-ultra-sneaky n 1))
  ([n c] (let [one (long 1)]
           (loop [n (long n) c (long c)]
             (if (= n one) c
                 (recur (long (if (= 0 (bit-and n 0x1))
                                (/ n 2)
                                (+ 1 (* 3 n))))
                        (inc c)))))))


(defn vs [start len]
  (let [a start b (+ a (dec len))]
    (timevs
     (collatz-max-cycle-length-sneaky a b)
     (collatz-max-cycle-length-ultra-sneaky a b))))

(vs 1 1000) [2.548752949646626 237.44262 93.160312 179 179]






(defn take-until
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (list (first s))
       (cons (first s) (take-until pred (rest s)))))))

(defn collatz-cycle [n]
  (take-until #(= % 1) (iterate collatz n)))








(use 'simple-plotter)
(create-window)

(cls)
(let [scale 0.1 xscale 10]
  (doseq [n (next (range 100))]
    (plot 0 (* scale n))
    (doseq [[i j] (partition 2 (interleave (next (range)) (collatz-cycle n)))]
      (draw-to (* xscale i) (* scale j)))))
  












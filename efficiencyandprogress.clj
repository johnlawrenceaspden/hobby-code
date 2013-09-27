;; Efficiency And Progress

;; Clojure is very slow:

(time (reduce + (map + (range 1000000) (range 1000000))))
"Elapsed time: 5316.638869 msecs"
;-> 999999000000

;; By comparison with C, we are looking at about 16ms to add two
;; vectors 1000000 long and then add up all the elements in the
;; result.

;; All my attempts to do this by writing sane programs fail miserably.

(defn tlrange[n lst]
  (if (zero? n) lst
      (recur (dec n) (cons n lst))))

(time (def lsta (tlrange 1000000 '())))
"Elapsed time: 394.066453 msecs" ;; speedy!
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 ...)
(time (def lstb (tlrange 1000000 '())))
"Elapsed time: 4665.077033 msecs" ;; wtf??

(time (reduce + lsta))
"Elapsed time: 1094.044291 msecs"
500000500000

;; Even when I resort to totally underhanded tactics:
(defn cheat [lst1 lst2 acc]
  (if (empty? lst1) acc
      (recur (rest lst1) (rest lst2) (+ acc (first lst1) (first lst2)))))

(time (cheat lsta lstb 0))
"Elapsed time: 1787.984422 msecs"
1000001000000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We can make arrays in the underlying Java though

(time (def a (int-array (range 1000000))))
"Elapsed time: 2004.260737 msecs"
(time (def b (int-array (range 1000000))))
"Elapsed time: 1563.989388 msecs"

;; And can operate on them
(defn asum [^ints xs]
  (areduce xs i ret (int 0)
    (+ ret (aget xs i))))

;; rather more quickly
(time (asum a))
"Elapsed time: 82.229171 msecs" ;; whee!
499999500000

;; It turns out that areduce is a macro
(macroexpand '(areduce xs i ret (int 0)
                       (+ ret (aget xs i))))

;; And so we can analyse its magic
(defn my-asum [^ints xs]
  (let* [a__4717__auto__ xs] 
        (clojure.core/loop [i 0 ret (int 0)] 
          (if (clojure.core/< i (clojure.core/alength a__4717__auto__))
            (recur (clojure.core/unchecked-inc i) 
                   (+ ret (aget xs i))) ret))))

;; And in fact slightly improve it
(defn my-asum [^ints xs]
  (let [N (clojure.core/alength xs)]
    (clojure.core/loop [i 0 ret (int 0)] 
      (if (clojure.core/< i N)
        (recur (clojure.core/unchecked-inc i) 
               (+ ret (aget xs i))) ret))))


(time (my-asum a))
"Elapsed time: 71.333233 msecs" ;; double whee!
499999500000

;; And make it a spot more readable.
(defn my-asum [^ints xs]
  (let [N (alength xs)]
    (loop [i 0 ret 0] 
      (if (>= i N) ret
        (recur (unchecked-inc i) (+ ret (aget xs i)))))))

(time (my-asum a))
"Elapsed time: 72.824274 msecs"
499999500000

(defn my-asum [^ints xs]
  (let [N (alength xs)]
    (loop [i 0 ret 0] 
      (if (>= i N) ret
          (recur (unchecked-inc i) (+ ret (aget xs i)))))))

(time (my-asum a))
"Elapsed time: 72.824274 msecs"
499999500000


;; What about adding two vectors?

;; Here's a truly horrid way to do it

(time (def c (int-array 1000000)))
"Elapsed time: 4.698991 msecs"

(aget c 0) ; 0
(aget c 999999) ; 0

(defn evil-amap [^ints xs ^ints ys ^ints cs]
  (let [N (dec (clojure.core/alength xs))]
    (loop [i 0]
      (aset cs i (+ (aget xs i) (aget ys i)))
      (if (< i N) (recur (unchecked-inc i))))))

(time (evil-amap a b c))
"Elapsed time: 188.844755 msecs" ;; 77 with *unchecked-math*

(aget c 0) ;-> 0
(aget c 999999) ;-> 1999998

;; Better perhaps is:

(defn my-amap-1 [^ints xs ^ints ys]
  (let [N (dec (clojure.core/alength xs))
        c (int-array (clojure.core/alength xs))]
    (loop [i 0]
      (aset c i (+ (aget xs i) (aget ys i)))
      (if (>= i N) c
        (recur (unchecked-inc i))))))

;; Which, is not enormously slower even though it's allocating a whole
;; new array and initializing it pointlessly to zero.
(def c (time (my-amap-1 a b)))
"Elapsed time: 192.613251 msecs" 

(aget c 0) ;-> 0
(aget c 999999) ;-> 1999998

;; This is even nicer I think
(defn my-amap [^ints xs ^ints ys]
  (let [c (int-array (clojure.core/alength xs))]
    (dotimes[i (clojure.core/alength xs)]
      (aset c i (+ (aget xs i) (aget ys i))))
    c))

(def c (time (my-amap a b)))
"Elapsed time: 190.621796 msecs" ;; 74

(aget c 0) ;-> 0
(aget c 999999) ;-> 1999998

;; It actually speeds up a bit with arrays of longs

(time (def a (long-array (range 1000000))))
"Elapsed time: 2004.260737 msecs"
(time (def b (long-array (range 1000000))))
"Elapsed time: 1563.989388 msecs"

(defn my-amap [^longs xs ^longs ys]
  (let [c (long-array (clojure.core/alength xs))]
    (dotimes[i (clojure.core/alength xs)]
      (aset c i (+ (aget xs i) (aget ys i))))
    c))

(def c (time (my-amap a b))) 
"Elapsed time: 176.998556 msecs" 

(aget c 0) ;-> 0
(aget c 999999) ;-> 1999998

;; this is my best shot at adding up two arrays


;; So let's try the reduce with longs too.
(defn my-asum [^longs xs]
  (let [N (alength xs)]
    (loop [i 0 ret 0] 
      (if (< i N)
        (recur (unchecked-inc i) (+ ret (aget xs i))) 
        ret))))

;; Doesn't seem enormously faster, but it's not slower, at least.

(time (my-asum a))
"Elapsed time: 71.168337 msecs"
499999500000

;; Let's try to replicate the C program

(def a (long-array (range 1000000)))
(def b (long-array 1000000))

(time (loop [count 1000 b b sum 0]
        (let [b (my-amap a b)
              s (+ sum (my-asum b))]
          (if (= 1 count) s
              (recur (dec count) b s)))))

"Elapsed time: 274035.291017 msecs"
250249749750000000

;; The right answer but 20x slower


;; A final flourish is to turn off overflow checking, and paranoid check for java reflection warnings
(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(defn my-amap [^longs xs ^longs ys]
  (let [c (long-array (clojure.core/alength xs))]
    (dotimes[i (clojure.core/alength xs)]
      (aset c i (+ (aget xs i) (aget ys i))))
    c))

(defn my-asum [^longs xs]
  (let [N (alength xs)]
    (loop [i 0 ret 0] 
      (if (< i N)
        (recur (unchecked-inc i) (+ ret (aget xs i))) 
        ret))))

(def a (long-array (range 1000000)))
(def b (long-array 1000000))

(time (loop [count 1000 b b sum 0]
        (let [b (my-amap a b)
              s (+ sum (my-asum b))]
          (if (= 1 count) s
              (recur (dec count) b s)))))
tx


;; produces this cryptic warning:

;;form-init3315407118030771353.clj:5 recur arg for primitive local: sum is not matching primitive, had: Object, needed: long
;;Auto-boxing loop arg: sum

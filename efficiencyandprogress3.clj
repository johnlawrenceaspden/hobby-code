;; Efficiency and Progress III : How close can we get to C speeds?

;; When messing about trying to make clojure quick, it's often well to have
(set! *warn-on-reflection* true)

;; A kind commenter on my previous post (Bernard) made me aware of the -march=native option for gcc
;; which allows it to optimize for the particular platform it's compiled on.

;; On my little netbook, this doubles the speed of the C program to
;; 8.6 seconds, so it looks like C can add two vectors of a million
;; integers to one another, and then add all the entries in the
;; result, in around 9 milliseconds.

;; A similar program in idiomatic clojure looks like:

(time (reduce + (map + (range 1000000) (range 1000000)))) ;-> 999999000000
"Elapsed time: 4689.303927 msecs"

;; So I figure that that makes clojure about 550 times slower than C
(/ 4689 8.6) ;-> 545.232558139535


;; I've had various suggestions about how this could be speeded up. 
;; Mark proved that it's not the adding up that's giving Clojure the trouble.
(time 
 (loop [i 0
        a 0]
   (if (= i 1000000) a (recur (inc i) (+ a i i))))) ;-> 999999000000
"Elapsed time: 65.885615 msecs"

;; But of course that's not really fair, since it's not adding up the big vectors

;; After a bit of reading round, it appears that the only way to get
;; this sort of thing to run fast in clojure is to use java's
;; primitive arrays:

;; It's quick to create these arrays. (Not that that really matters)
(time (def a (int-array 1000000)))
"Elapsed time: 6.096021 msecs"
(time (def b (int-array 1000000)))
"Elapsed time: 6.539166 msecs"

;; And you can iterate over them using dotimes and aset, C-style.

;; Do this naively and it's insanely slow
(time (dotimes [i 1000000] (aset a i i)))
;; this is where *warn-on-reflection* pays off : Reflection warning: call to aset can't be resolved.
"a long time"


;; What a difference a type-hint makes!
(time 
 (let [a ^ints a]
   (dotimes [i 1000000] (aset a i i))))
"Elapsed time: 108.186453 msecs"

;; We're still in clojure, which is nice:
(take 10 a) ;-> (0 1 2 3 4 5 6 7 8 9)

;; dotimes is a macro, so we can have a look at what it's doing
(macroexpand '(dotimes [i 1000000] (aset a i i)))

;; And by analogy construct loops of our own
(time 
 (let [a ^ints a b ^ints b]
   (loop [i 0] 
     (when (< i 1000000) 
       (aset a i i)
       (aset b i i)
       (recur (unchecked-inc i))))))
"Elapsed time: 185.52059 msecs"

;; Here we do a vector addition (eek! mutation!)
(time 
 (let [a ^ints a b ^ints b]
   (loop [i 0] 
     (when (< i 1000000) 
       (aset b i (+ (aget a i)(aget b i)))
       (recur (unchecked-inc i))))))
"Elapsed time: 229.882722 msecs"

(take 10 b) ;-> (0 2 4 6 8 10 12 14 16 18)

;; and reduction
(time 
 (let [b ^ints b]
   (loop [i 0 sum 0] 
     (if (< i 1000000)
       (recur (unchecked-inc i) (+ sum (aget b i)))
       sum))))

"Elapsed time: 84.701136 msecs"
999999000000



;; So let's bite the bullet and see whether we can do the computation that C did.
;; I'm estimating about 5 minutes rather than 9 seconds, but it's progress.


;; This is a bit of a counsel of despair, but to be fair to clojure:
(set! *unchecked-math* true)

(def N 1000000)
(def a (int-array (range N)))
(def b (int-array N))

(time 
 (let [a ^ints a b ^ints b N ^long N]
   (loop [count 0 sum 0]
     (if (= count 1000) sum
         (do 
           (println count sum)
           (dotimes [i N] (aset b i (+ (aget a i)(aget b i))))
           (recur (inc count) (+ sum (loop [i 0 ret 0] 
                                       (if (= i N) ret
                                           (recur (unchecked-inc i) (+ ret (aget b i))))))))))))
"Elapsed time: 502988.212333 msecs"                                    
250249749750000000

(def N 1000000)
(def a (int-array (range N)))
(def b (int-array N))


(time 
 (let [a ^ints a b ^ints b]
   (loop [count 0 sum 0]
     (if (= count 1000) sum
         (do 
           (println count sum)
           (dotimes [i N] (aset b i (+ (aget a i)(aget b i))))
           (recur (inc count) (+ sum (areduce b i ret 0 (+ ret (aget b i))))))))))

     
 "Elapsed time: 126538.750983 msecs"
250249749750000000
   

(def N 1000000)
(def a (int-array (range N)))
(def b (int-array N))


(time 
 (let [a ^ints a b ^ints b]
   (loop [count 0 sum 0 b ^ints b]
     (if (= count 1000) sum
         (do 
           (println count sum)
           (let [b ^ints (amap b i ret (+ (aget a i) (aget b i)))]
             (recur (inc count) (+ sum (areduce b i ret 0 (+ ret (aget b i)))) b)))))))
    



 












;; There are actually helpful macro for doing such things
(time 
 (let [b ^ints b]
   (areduce b i ret 0 (+ ret (aget b i)))))
"Elapsed time: 100.587301 msecs"
999999000000

;; here's the map version. It's a bit difficult to understand what it
;; does unless you're used to constructing the loops by hand
(time 
 (let [b ^ints b a ^ints a]
   (amap b i ret (+ (aget a i) (aget b i)))))



(macroexpand '(amap b i ret (+ (aget a i) (aget b i))))

;; Efficiency and Progress V:

;; Recap

;; This is slow:
(time (reduce + (map + (range 1000000) (range 1000000))))

;; C can perform roughly equivalent task in 8.6 ms, and Java in around 20 ms

;; So do we have to drop into C or Java when we want to make algorithms fast?
;; I hope not! 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I'm starting clojure like this, so that I know it's running as fast as it can
;; rlwrap java -server -classpath ~/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar:. clojure.main

(clojure-version) ;-> "1.5.1"
((into{} (System/getProperties)) "java.version") ;-> "1.7.0_25"
((into{} (System/getProperties)) "java.vm.name") ;-> "OpenJDK Server VM"
((into{} (System/getProperties)) "sun.management.compiler") ;-> "HotSpot Tiered Compilers"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeated runs through this little benchmark now show 
;; Hotspot doing its thing and speeding up the calculation
(time (reduce + (map + (range 1000000) (range 1000000))))
999999000000
"Elapsed time: 2852.570643 msecs"
"Elapsed time: 2756.410014 msecs"
"Elapsed time: 1907.089513 msecs"
"Elapsed time: 1872.189534 msecs"
"Elapsed time: 1870.054495 msecs"
"Elapsed time: 1907.627285 msecs"

;; Setting these two variables is a good thing when trying to achieve C/Java like speeds
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; And doesn't seem to make any difference to this code
(time (reduce + (map + (range 1000000) (range 1000000))))
999999000000
"Elapsed time: 1883.237194 msecs"

;; So I think it's safe to conclude that Clojure written idiomatically is around 100x slower than Java

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My best shot so far is:

(def N 1000000)
(def a (int-array (range N)))
(def b (int-array N))

(time 
 (let [a ^ints a b ^ints b N ^int N]
   (loop [count 0 sum 0]
     (if (= count 1000) sum
         (do 
           (println count sum)
           (dotimes [i N] (aset b i (+ (aget a i)(aget b i))))
           (recur (inc count) (+ sum (loop [i 0 ret 0] 
                                       (if (= i N) ret
                                           (recur (unchecked-inc i) (+ ret (aget b i))))))))))))

;; Bernard suggests:

(time
 (let [a ^ints a b ^ints b N ^int N]
   (loop [count (long 0) sum (long 0)]
     (if (== count 1000) sum
         (do
           ;; (println count sum)
           (let [b (amap  a idx ret (+ (aget a idx) (aget b idx)))]
             (recur (inc count) (areduce ^ints b i res (long sum) (unchecked-add res (aget ^ints b i))))))))))


;; Dmitry Groshev:

(defn test3 []
(let [^ints a a
^ints b b
N (int N)]
(loop [count (int 0) sum (long 0)]
(if (== count 1000) sum
(do
(loop [i (int 0)]
(when (< i N)
(aset b i (+ (aget a i) (aget b i)))
(recur (inc i))))
(recur (inc count)
(+ sum (long (loop [i (int 0) ret (long 0)]
(if (== i N) ret
(recur (inc i)
(+ ret (aget b i)))))))))))))


(defmacro c-for
"C-like loop with nested loops support"
[loops & body]
(letfn [(c-for-rec [loops body-stmts]
(if (seq loops)
(let [[var init check next] (take 4 loops)]
`((loop [~var ~init]
(when ~check
~@(c-for-rec (nthrest loops 4) body-stmts)
(recur ~next)))))
body-stmts))]
`(do ~@(c-for-rec loops body) nil)))

This way

(loop [i (int 0)]
(when (< i N)
(aset b i (+ (aget a i) (aget b i)))
(recur (inc i))))

becomes

(c-for [i (int 0) (< i N) (inc i)]
(aset b i (+ (aget a i) (aget b i))))



;; And James Reeves, by private e-mail after he couldn't comment on this blog

(defn asum [^ints xs]
  (areduce xs i s 0 (unchecked-add s (aget xs i))))

(defn amap-add [^ints xs ^ints ys]
  (dotimes [i (alength xs)]
    (aset xs i (unchecked-add (aget xs i) (aget ys i)))))

(defn test-low-level []
  (let [a (int-array 1000000)
        b (int-array 1000000)]
    (dotimes [i (alength a)]
      (aset a i i))
    (loop [count 0, sum 0]
      (amap-add b a)
      (if (< count 1000)
        (recur (unchecked-inc count) (unchecked-add sum (asum b)))
        sum))))

(defn -main []
  (time (println (str "sum=" (test-low-level)))))





























;; Sadly the code is completely unreadable, but we may be able to do something about that. 
;; We are in a lisp after all:

;; It turns out there are macros for doing this sort of thing:

(def N 1000000)
(def a (int-array (range N)))
(def b (int-array N))

(time 
 (let [a ^ints a]
   (areduce a i ret 0 (+ ret (aget a i)))))

"Elapsed time: 46.748545 msecs"
499999500000

;; It's worth examining what areduce is doing
(macroexpand '(areduce a i ret 0 (+ ret (aget a i))))
;; (let* [a__4717__auto__ a] (clojure.core/loop [i 0 ret 0] (if (clojure.core/< i (clojure.core/alength a__4717__auto__)) (recur (clojure.core/unchecked-inc i) (+ ret (aget a i))) ret)))

(time (let [a ^ints a]
        (let* [a__4717__auto__ a] 
              (clojure.core/loop [i 0 ret 0] 
                (if (clojure.core/< i (clojure.core/alength a__4717__auto__)) 
                  (recur (clojure.core/unchecked-inc i) (+ ret (aget a i))) ret)))))
"Elapsed time: 46.674857 msecs"

;; edited for clarity
(time (let [a ^ints a] 
            (loop [i 0 ret 0] 
              (if (< i (alength a)) 
                (recur (unchecked-inc i) (+ ret (aget a i))) ret))))
"Elapsed time: 40.352545 msecs"
;; weirdly the editing seems to have actually speeded it up. I have absolutely no idea why.

;; And there's also amap, which seems weirdly slower, even though it's doing three array accesses instead of one.
(time (let [a ^ints a b ^ints b] 
        (amap a i ret (+ (aget b i)(aget a i)))))
"Elapsed time: 203.306294 msecs"
#<int[] [I@116fc35> ;; Apparently this means 'integer array'

;; It turns out that amap does what my iteration did but without mutation, by making a copy
(macroexpand '(amap a i ret (+ (aget b i)(aget a i))))

(time 
 (let [a ^ints a b ^ints b]
   (let [ret (clojure.core/aclone a)] 
     (loop [i 0] 
       (if (< i (alength a)) 
         (do (clojure.core/aset ret i (+ (aget b i) (aget a i))) 
             (recur (clojure.core/unchecked-inc i))) 
         ret)))))
"Elapsed time: 187.263691 msecs"
#<int[] [I@b3f01d> ;; Again, this seems to have got slightly faster just because I tidied it up.




;; Let's try the long calculation again using amap and areduce

(time
 (let [a ^ints a b ^ints b]
   (loop [count 0 sum 0 b b]
     (if (< count 1000)
       (do (println count sum)
           (let [b (amap a i ret (+ (aget b i) (aget a i)))]
             (let [sum (+ sum (areduce b i ret 0 (+ ret (aget b i))))]
               (recur (unchecked-inc count) sum b))))
       sum))))

;; This just freezes solid and I've no idea why. I can't actually understand it.



(time
 (let [a ^ints a b ^ints b]
   (loop [count 0 sum 0 b b]
     (if (= count 1000) sum
         (do (println count sum)
             (recur (unchecked-inc count) (+ sum (areduce b i ret 0 (+ ret (aget b i)))) (amap a i ret (+ (aget b i) (aget a i)))))))))





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
    
form-init4591430370612349788.clj:8 recur arg for primitive local: sum is not matching primitive, had: Object, needed: long
Auto-boxing loop arg: sum

"Elapsed time: 59580.516979 msecs"
250249749750000000


 












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

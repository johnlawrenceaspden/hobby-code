

;; And another after reading everyone else's helpful code
(def N 1000000)
(def a (int-array (range N)))
(def b (int-array N))


(time 
 (let [a ^ints a b ^ints b]
   (loop [count 0 sum 0]
     (if (= count 1000) sum
         (do 
           (println count sum)
           (dotimes [i N] (aset b i (unchecked-add (aget a i)(aget b i))))
           (recur (unchecked-inc count) (unchecked-add sum (areduce b i ret 0 (+ ret (aget b i))))))))))

"Elapsed time: 66040.099305 msecs"
250249749750000000

;; This is roughly the same speed, but you don't have to use *unchecked-math*







;;And Dmitry again:

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

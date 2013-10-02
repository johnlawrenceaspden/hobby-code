;; Efficiency and Progress V:

;; Recap

;; This is slow:
(time (reduce + (map + (range 1000000) (range 1000000))))

;; C can perform roughly equivalent task in 8.6 ms, and Java in around 16 ms

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

;; I have a little microbenchmark written in C which represents the
;; sort of things I am trying to do by adding length 1000000 vectors
;; and then adding up all the numbers in the vectors repeatedly.

;; Appropriately compiled, it runs in 8.6 seconds

;; Approximately the same program, translated into Java, runs in
;; around 16 seconds with java7 -server

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In idiomatic Clojure I reckone this program would take about 1600
;; seconds, or about half an hour. And that's actually reflective of
;; the difference between C and Clojure that I've observed while
;; trying to write algorithms code as part of the Coursera/Stanford
;; Algorithms II class (which I very highly recommend!)

;; My best shot in non-idiomatic Clojure so far is:

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

"Elapsed time: 177181.749304 msecs"
250249749750000000
;; Which is unreadable and still about 11x slower than Java, but gets the right answer

;; I am so unused to using mutation in Clojure that I keep forgetting
;; to reset the variables and then being surprised when the answers
;; are wrong. It really screws up the REPL way of programming! No
;; wonder LISP developed the functional style that's now becoming so
;; fashionable.



;; After an awful lot of essentially random screwing around, I managed
;; to concoct another version, which uses the areduce macro instead of
;; the explicit inner loop above.
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

"Elapsed time: 63657.893856 msecs"
250249749750000000

;; I have no idea why this runs so much faster. A profiler would be
;; very useful here, but jvisualvm, which I used to find really useful
;; when tuning clojure, is giving me no real information and wasting
;; an awful lot of time in return. It only seems to profile at the
;; level of Java classes, and so this loop code isn't visible to it.

;; Still, down to about 4x slower than Java, maybe 7x slower than C,
;; and (I think) quite a lot more readable.



;; Various commenters have been kind enough to suggest improvements over my pitiful effort.

;; Dmitry Groshev's:

(def N 1000000)
(def a (int-array (range N)))
(def b (int-array N))

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


(time (test3))

"Elapsed time: 45213.470027 msecs"
250249749750000000

;; Gets down to 3x slower than Java, 5x slower than C




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

(time (test-low-level))
"Elapsed time: 46496.528226 msecs"
250249749750000000



;; Has managed to get down to the same speed, whilst splitting the
;; inner loops off into nice little functions!

;; I think this version wins hands down for general comprehensibility.

;; So well done James, this is this blog's readership's collective
;; best shot at this problem so far.

;; We're down to 3x slower than Java, 5x slower than C, and readable
;; if not quite as readable as it would be in a language designed for
;; imperative loops over arrays.

;; If I can learn to produce code like this reliably and without too
;; much buggering about, then I may be able to stay in Clojure for the
;; tight loops parts of my algorithms code. A factor of 5 I can
;; tolerate for the convenience of doing everything else in Clojure.

;; As Dmitry pointed out, it may be possible to use macros to make a
;; little language to make this easier

;; And James shows us another possible route with functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I still don't understand why these similar looking bits of code run
;; at such different speed, and that worries me, because that's going
;; to translate to a lot of screwing around while trying to write such
;; things.

;; And also, why even after all this are we not as fast as Java?

;; Bernard said something about 'removing synchronization', which
;; sounds scary.

;; Another approach, of course, would be to write pure Java classes to
;; do the heavy lifting and call them from Clojure code which does the
;; pre-processing, but that sounds like a fairly nasty and fragile
;; approach itself, and I'd rather avoid doing that if I can. But it
;; should be a bit easier than using C within python, at least.

;; I should also investigate core.matrix and hiphip, two projects
;; targeted at this sort of thing.

;; If anyone knows how to use them to solve this problem, please chip in.


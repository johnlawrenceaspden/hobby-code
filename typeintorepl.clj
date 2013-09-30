;; LEIN_CLASSPATH=`lein classpath`
;; rlwrap java -classpath $LEIN_CLASSPATH clojure.main -e "( do (require 'clojure.tools.nrepl.server) (clojure.tools.nrepl.server/start-server :bind \"127.0.0.1\"))" -r

;; rlwrap java -classpath $LEIN_CLASSPATH clojure.main 

;; rlwrap java -classpath ~/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar clojure.main -e "( * 2 3 )"

;; rlwrap java -classpath ~/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar clojure.main

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


NO_SOURCE_FILE:27 recur arg for primitive local: sum is not matching primitive, had: Object, needed: long
Auto-boxing loop arg: sum

;; Mark Engleberg
(time (loop [i 0 a 0]
        (if (= i 1000000) a (recur (inc i) (+ a i i)))))

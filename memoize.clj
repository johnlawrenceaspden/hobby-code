;;I noticed the other day that clojure has built in support for memoization,
;;which is a technique for remembering the return values of functions which
;;take a while to calculate

;;The classic example of when this can be useful is 
;;the naive fibonnacci calculation.

;;When expressed as the obvious recursion, this is a very bad algorithm indeed.
;;Memoization turns it into a much better algorithm

;;I'll let the code speak for itself. You get a not inconsiderable speedup.

;;Note the nasty technique of mutating the value of the variable fib.

(defmacro time-it  [expr]
  `(let [start# (. System (nanoTime))
         result#   ~expr
         finish# (. System (nanoTime))]
      (/ (double (- finish# start#)) 1000000.0)))

(defn fib [n]
  (if (< n 2) n
      ( + (fib (- n 1)) (fib (- n 2)))))

(doall (map #(time-it (fib %)) (range 1 30)))

(def fib (memoize fib))

(doall (map #(time-it (fib %)) (range 1 30)))
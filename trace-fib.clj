(defn fib [n]
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(use 'clojure.contrib.trace)
(dotrace (fib) (fib 3))
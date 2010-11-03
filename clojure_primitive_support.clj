(defn fib [n]
  (if (<= n 1)
    1
    (+ (fib (dec n)) (fib (- n 2)))))

(time (fib 38))
;;24716.88 ms in clojure 1.2 on mum's computer
;;20800 ms in 1.3.0-alpha2

(defn ^:static fib ^long [^long n]
  (if (<= n 1)
    1
    (+ (fib (dec n)) (fib (- n 2)))))
;;2112 ms in 1.3.0-alpha2

(defn factorial[n]
  (if (< n 2) 1N (* n (factorial (dec n)))))

(factorial 1000)

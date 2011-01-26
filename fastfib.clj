(defn fib [n]
  (if (< n 2) n
      (+ (fib (dec n)) (fib (dec (dec n))))))

;; Clojure 1.2
(time (fib 32)) ; "Elapsed time: 3300.189899 msecs"
2178309

;; Clojure 1.3.0-alpha4
(time (fib 32)) ; "Elapsed time: 3240.706087 msecs"
2178309

(defn ^long fib [^long n]
     (if (< n 2) n
         (+ (fib (dec n)) (fib (dec (dec n))))))

;; Clojure 1.3.0-alpha4
(time (fib 32)) ; "Elapsed time: 1059.60362 msecs"
2178309

;; Clojure 1.3.0-alpha4
(time (fib 35)) ; "Elapsed time: 4259.415891 msecs"
9227465


(defn ^long fib [^long n]
     (if (< n 2) n
         (+ (fib (dec n)) (fib (- n 2))))) ; #'user/fib

;; Clojure 1.3.0-alpha4
(time (fib 35)) ; "Elapsed time: 4478.61122 msecs"
9227465

(defn fib [n]
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))


(fib 3)

(map fib (range 10)) ; (0 1 1 2 3 5 8 13 21 34)
(map fib (range 10 20)) ; (55 89 144 233 377 610 987 1597 2584 4181)
(map fib (range 20 30)) ; (6765 10946 17711 28657 46368 75025 121393 196418 317811 514229)
(fib 32) ; 2178309
(fib 33) ; 3524578

(def fib (memoize fib))

(fib 90)

(defn fib-iter [n a b]
  (if (= n 0) b
      (fib-iter (- n 1) (+ a b) a)))


(fib-iter 0 1 0)
(fib-iter 90 1 0)

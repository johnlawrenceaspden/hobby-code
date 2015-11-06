#lang racket

(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))


;; 323 / 357

34 323
289 34
255 34
221 34
187 34
153 34
119 34
85 34
51 34
17 34
17 17

(define (gcd a b)
  (printf "gcd ~a ~a\n" a b)
  (if (= a b) a
      (if (< a b) (gcd a (- b a))
          (gcd (- a b) b))))

(gcd 17 16)

(gcd 357 323)
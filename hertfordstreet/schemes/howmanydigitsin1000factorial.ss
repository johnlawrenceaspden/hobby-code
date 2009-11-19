#lang scheme

;how many digits in 1000!

(define (logfactorial n)
  (if (= n 1) 0
      ( + (log n) (logfactorial (- n 1)))))

(/(logfactorial 1000)(log 10))


(define (factorial n)
  (if (= n 1) 1
      (* n (factorial (- n 1)))))

(/(log (factorial 1000))(log 10))

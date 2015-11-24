#lang scheme

(define (factorial n)
  (if (< n 2) n
      (* n (factorial (- n 1)))))

(factorial 10)


#lang scheme
(require test-engine/scheme-tests)
(define (double x)
  (* x 2))

(check-expect (double 10) 20)
(check-expect (double 15) 31)

(test)
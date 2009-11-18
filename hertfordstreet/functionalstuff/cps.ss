#lang scheme

(define cc-list '())

(define (main)
     (* (+ 1 2) (+ 2 3)))

(display (main))

(define (cps+ a b cont) 
  (cont (+ a b)))

(define (cps* a b cont)
  (set! cc-list (append cc-list (list cont)))
  (cont (* a b)))

(define (cpsmain cont)
    (cps+ 1 2 (λ(r)
                 (cps+ 2 3 (λ(s) 
                             (cps* s r cont))))))

(cpsmain (λ(x) (display x)))

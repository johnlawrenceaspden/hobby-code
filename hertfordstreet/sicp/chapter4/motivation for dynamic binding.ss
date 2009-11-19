#lang scheme

(require (lib "trace.ss"))

(define sum
  (λ (term a next b)
    (cond ((> a b) 0)
          (else (+ (term a)
                   (sum term (next a) next b))))))

(define product
  (λ (term a next b)
    (cond ((> a b) 1)
          (else (* (term a)
                   (product term (next a) next b))))))

(define 1+ (λ(x) (+ x 1)))

(define sum-powers
  (λ (a b n)
    (sum (λ(x) (expt x n)) a 1+ b)))

(define product-powers
  (λ (a b n)
    (product (λ(x) (expt x n)) a 1+ b)))

(sum-powers 1 5 3)     (+ (* 1 1 1) (* 2 2 2) (* 3 3 3) (* 4 4 4) (* 5 5 5))
(product-powers 1 5 3) (* (* 1 1 1) (* 2 2 2) (* 3 3 3) (* 4 4 4) (* 5 5 5))

#| this seems an obvious abstraction, but of course it doesn't work, because n has no meaning
(define nthpower (λ(x) (expt x n)))

(define sum-powers2
  (λ (a b n)
    (sum nthpower a 1+ b)))

(define product-powers2
  (λ (a b n)
    (product nthpower a 1+ b)))
|# 

;dynamic binding is a way to make this work. 

;here's an abstraction we can make with lexical scope
(define (nthpower n) (λ(x) (expt x n)))

(define sum-powers2
  (λ (a b n)
    (sum (nthpower n) a 1+ b)))

(define product-powers2
  (λ (a b n)
    (product (nthpower n) a 1+ b)))

(sum-powers2 1 5 3)
(product-powers2 1 5 3)


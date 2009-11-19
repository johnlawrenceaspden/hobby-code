#lang scheme

(require (planet "main.ss" ("schematics" "schemeunit.plt" 3 3)))

(define (tagged-list? a l) (eq? (car l) a))

(define (variable? exp) (symbol? exp))
(define (same-variable? exp var) (eq? exp var))

(define (make-sum a b) `(+ ,a ,b))
(define (sum? exp) (tagged-list? '+ exp))
(define (addend exp) (cadr exp))
(define (augend exp) (caddr exp))

(define (make-product a b) `(* ,a ,b))
(define (product? exp) (tagged-list? '* exp))
(define (multiplier exp) (cadr exp))
(define (multiplicand exp) (caddr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))))
        (else (error "unknown expression type -- DERIV" exp))))

(deriv (make-product 'x 'x) 'x)
(deriv (deriv (make-product 'x 'x) 'x) 'x)
(deriv (make-product 'x 'x) 'y)

(check-equal? (deriv (make-product 'x 'x) 'x) '(+ (* 1 x) (* x 1)))
(check-equal? (deriv (deriv (make-product 'x 'x) 'x) 'x) '(+ (+ (* 0 x) (* 1 1)) (+ (* 1 1) (* x 0))))
(check-equal? (deriv (make-product 'x 'x) 'y) '(+ (* 0 x) (* x 0)))
(load "putget.scm")
(require (lib "trace.ss"))


(define (atom? exp) (not (pair? exp)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? exp) (and (atom? exp) (not (number? exp))))
(define (same-variable? exp var) (and (atom? exp) (eq? exp var)))
(define (make-sum a b) (list '+ a b))
(define (make-product a b) (list '* a b))
(define (make-difference a b) (list '- a b))
(define (make-quotient a b) (list '/ a b))
(define (make-logarithm a) (list 'log a))
(define (make-exponentiation a b) (list 'expt a b))


;------------sums---------------------
(define (deriv-sum args var)
  (make-sum (deriv (car args) var) (deriv (cadr args) var)))
(put '+ 'deriv deriv-sum)

;---------------products---------------
(define (deriv-product args var)
  (make-sum (make-product (deriv (car args) var) (cadr args)) (make-product (car args) (deriv (cadr args) var))))
(put '* 'deriv deriv-product)

;--------------differences-------------
(define (deriv-diff args var)
  (make-difference (deriv (car args) var) (deriv (cadr args) var)))
(put '- 'deriv deriv-diff)

;--------------quotients---------------
(define (deriv-quotient args var)
  (make-quotient 
   (make-difference 
    (make-product (deriv (car args) var) (cadr args)) 
    (make-product (car args) (deriv (cadr args) var)) )
   (make-product (cadr args) (cadr args))))
(put '/ 'deriv deriv-quotient)

;---------------logarithms---------------
(define (deriv-logarithm args var)
  (make-product (make-quotient 1 (car args)) (deriv (car args) var)))
(put 'log 'deriv deriv-logarithm)


;---------exponents (expt a b)-----------

(define (deriv-exponentiation args var)
  (let ((a (car args))
        (b (cadr args)))
    (make-sum
     (make-product
      (make-product (make-exponentiation a b) (make-logarithm a))
      (deriv b var))
     (make-product
      (make-product b (make-exponentiation a (make-difference b 1)))
      (deriv a var)))))

(put 'expt 'deriv deriv-exponentiation)




(trace deriv)
(deriv '(expt x x) 'x)
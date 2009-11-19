(define (cons x y)
  (lambda (m) (m 
               x
               y
               (lambda (n) (set! x n))
               (lambda (n) (set! y n)))))

(define (car x)
  (x (lambda (a d sa sd) a)))

(define (cdr x)
  (x (lambda (a d sa sd) d)))

(define (set-car! x y)
  ((x (lambda (a d sa sd) sa)) y))

(define (set-cdr! x y)
  ((x (lambda (a d sa sd) sd)) y))

;---------------------------------------

(define a (cons 1 3))
(car a)
(cdr a)
(set-car! a 4)
(car a)
(cdr a)
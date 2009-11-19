(define (cons x y)
  (lambda (m) (m x y)))

(define (car a) (a (lambda (x y) x)))
(define (cdr a) (a (lambda (x y) y)))


(define a (cons 1 2))
(car a)
(cdr a)
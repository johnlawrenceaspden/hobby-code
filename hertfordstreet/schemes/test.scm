(define (l) (load "test"))

(define square (lambda (x) (* x x)))
(define (square x) (* x x))

(define sum-of-squares (lambda (x y) (+ (square x) (square y))) )
(define (sum-of-squares x y) (+ (square x)(square y)))

(define (f a) (sum-of-squares (+ a 1) (* a 2)))

;Normal Order (fully expand and then reduce)
(f 5)
( sum-of-squares (+ 5 1) (* 5 2))
(+ (square (+ 5 1) ) (square (* 5 2)) )
(+ (* (+ 5 1)(+ 5 1)) (* (* 5 2)(* 5 2)))
(+ (* 6 6) (* 10 10))
(+ 36 100)
136

;Applicative Order (expand operators and operands then evaluate)
(f 5)
( sum-of-squares (+ 5 1) (* 5 2))
( (lambda (x y) (+ (square x) (square y)) ) 6 10 )
(+ (square 6) (square 10))
(+ ( * 6 6) (* 10 10))
(+ 36 100)
136

(define (abs x) (cond ((< x 0)(- x))
                      ((= x 0) 0)
                      ((> x 0) x)))

(define (abs x) (if (< x 0) (- x) x))

(define (abs x) (cond ((< x 0) (- x))
                      (else x)))



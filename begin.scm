(define (factorial n) 
  (if (= n 0) 1 
      (* n (factorial (- n 1)))))

(define (average a b) (/ ( + a b) 2))
(define (absolute x) (if (< x 0) (- x) x))
(define (square x) (* x x))

; if the guess isn't good enough, improve it and try again
(define (iterative-improve guess improve good-enough?)
  (if (good-enough? guess) 
      guess 
      (iterative-improve (improve guess) improve good-enough?)))

(define (make-improver n)
  (lambda (x) (average x (/ n x))))

(define (make-good-enough n)
  (lambda (x) (< (absolute (- (square x) n)) 0.00000000001)))

(define (square-root n) 
  (iterative-improve 1. (make-improver n) (make-good-enough n)))

(square-root 10)
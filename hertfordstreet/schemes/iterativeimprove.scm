(define (iterative-improve good-enough? improve)
  (lambda (x) 
    (define (try x) (display x)(newline) (if (good-enough? x) x (try (improve x))))
    (try x)))

(define (issqrt? n) (lambda (x) (< (abs (- n (* x x))) 0.000000000000000000000000000000000000000001)))
(define (huron n) (lambda (x) (/(+ x (/ n x)) 2)))

(define (squareroot x) ((iterative-improve 
                         (issqrt? x) 
                         (huron x))
                        1))

;(squareroot 25)

(define (fixedpoint f first-guess) ((iterative-improve
                                     (lambda (x) ( < (abs (- x (f x))) 0.00001))
                                     (lambda (x) (f x))) 
                                     first-guess))

(fixedpoint sin 1)
(define (square x) (* x x))

(define (improve-guess x guess)
  (/ (+ guess (/ x guess)) 
     2))

(define (good-enough? x guess tolerance)
  ( < (abs (- (square guess) x)) 
      tolerance))

(define (sqrt-helper x guess tolerance)
  (display guess) (newline)
  (if (good-enough? x guess tolerance)
      guess
      (sqrt-helper x (improve-guess x guess) tolerance)))

(define (sqrt x)
  (sqrt-helper x (/ x 2.0) 0.00001))

(sqrt 10000000)
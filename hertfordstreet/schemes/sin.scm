(define (p x) (-(* 3 x)(* 4 x x x)))
(define (sine x)
  (if (< x 0.1) 
      x
      (p (sine (/ x 3)))))

(sine 5)


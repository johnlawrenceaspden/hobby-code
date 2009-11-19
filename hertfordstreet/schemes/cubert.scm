(define (l) (load "cubert"))






(define (cubert x) 
  
  (define (cube a) (* a a a))
  
(define (improve guess ) (/ (+ (/ x (* guess guess)) (* guess 2)) 3 ))
  
(define (error guess ) (abs(-(cube guess)x)) )
  
(define (good-enough guess ) (< (error guess ) 0.01) )

(define (try guess  )
  (if (good-enough guess  )
    guess 
    (try (improve guess )  )))

  (try 1.0))


















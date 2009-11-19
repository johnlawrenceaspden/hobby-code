(define ((double f)x) (f (f x)))

(define (square x) (* x x))

(square 2)

((double square) 2)

((double (double square)) 2)
(((double double) square) 2) 

((double (double (double square))) 2)
(((double (double double)) square) 2)


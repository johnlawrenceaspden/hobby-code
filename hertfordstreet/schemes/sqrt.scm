(define (l) (load "sqrt"))

(define (square x) (* x x))
(define (avg a b) (/ (+ a b) 2) )

(define (improve guess x) (avg guess (/ x guess)) )

(define (error guess x) (abs(-(square guess)x)) )

(define (goodenougherr guess x) (< (error guess x) 0.01) )

(define (goodenoughdelta guess x lastguess) (< (abs (- guess lastguess)) (/ guess 100)))


(define (tryerr guess x )
  (if (goodenougherr guess x )
    guess 
    (tryerr (improve guess x) x ))
  )

(define (trydelta guess x lastguess)
  (if (goodenoughdelta guess x lastguess)
    guess 
    (trydelta (improve guess x) x guess))
  )


(define (sqrtdelta x) (trydelta 1.0 x 2.0))
(define (sqrterr x) (tryerr 1.0 x))














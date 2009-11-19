;(define (exponent b n)
;  (if (= n 0)
;      1
;      (* b (exponent b (- n 1)))))
;
;(define (expt-iter total b n)
;  (if ( = n 0) total (expt-iter (* b total) b (- n 1))))
;
;(define (exponent b n)
;  (expt-iter 1 b n))

;(define (square x) (* x x))
;(define (exponent b n)
;  (cond ((= n 0) 1)
;        ((even? n) (square (exponent b (/ n 2))))
;        (else (* b (exponent b (- n 1))))))



(define (expt-iter total b n)
  (cond (( = n 0) total)
        ((even? n) (expt-iter total (* b b) (/ n 2)))
        ( else (expt-iter (* b total) b (- n 1)))))

(define (exponent b n)
  (expt-iter 1 b n))




(exponent #i1.00001 100000000)

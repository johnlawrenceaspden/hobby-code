;(define (* a b)
;  (if (= b 0)
;      0
;      (+ a (* a (- b 1)))))

;(define (*-iter total a b)
;  (if ( = b 0) total (*-iter (+ a total) a (- b 1))))
;
;(define (* b n)
;  (*-iter 0 b n))

(define (double x) (+ x x))
(define (halve x ) (/ x 2))

;(define (* a b)
;  (cond ((= b 0) 0)
;        ((even? b) (double (* a (halve b))))
;        (else (+ a (* a (- b 1))))))

(define (*-iter total a b)
  (cond (( = b 0) total)
        ((even? b) (*-iter total (double a) (halve b)))
        ( else (*-iter (+ a total) a (- b 1)))))

(define (* a b)
  (*-iter 0 a b))

(* 8 10000000)




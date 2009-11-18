#lang scheme

;we wish to define multi valued functions (returning lists)
(define (msqrt z)
  (let ((a (sqrt z)))
    (list a (- a))))

(msqrt 9)

;here's another
(define cubertminus1 (exp (* 0+i (* pi 2/3))))

(define (mcbrt z)
  (let ((a (expt z 1/3)))
    (list a (* a cubertminus1) (* a cubertminus1 cubertminus1) )))

(mcbrt 8)

;test it
(map (lambda (x) (* x x x)) (mcbrt 8))
(map (lambda (x) (* x x))   (msqrt 8))

;but how to compose? (msqrt (mcbrt (expt 2 6)) doesn't give us six values.

;bind should take a function double->list of double
;and return a function list of double->list of double
(define (bind f)
  (lambda (ld)
    (append-map f ld)))

;composition of functions
((bind mcbrt) (msqrt (expt 2 6)))

;identity (unit)
(define (unit x)
  (list x))

;example identity usages
((bind unit)
 ((bind msqrt)
  ((bind unit)
   ((bind msqrt)
    (unit (expt 2 4))))))

;composition and lifting
(define (compose f g)
  (lambda (x) ((bind f) (g x))))

(define (lift f)
  (lambda (x) (unit (f x))))

;;(cos (sqrt (cbrt (sin 2))))
((compose (lift cos)(compose (compose msqrt mcbrt) (lift sin))) 2)










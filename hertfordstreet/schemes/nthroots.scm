(define (power x a) (exp (* a (log x))))

(define (fixedpoint fn guess tolerance debug)
  (define (close-enough a b) (< (abs (- a b)) tolerance))
  (define (iter old new)
    (if debug (printf "new guess: ~s ~%" new) 0)
    (if (close-enough old new) new (iter new (fn new))))
  (iter guess (fn guess)))

(define (fixedsolved fn guess debug)
  (fixedpoint fn guess 0.000000001 debug))

(define (fixedsolve fn guess) (fixedsolved fn guess #f))
(define (fixedsolvedebug fn guess) (fixedsolved fn guess #t))

(define (average a b) (/(+ a b) 2))
(define (averagedamp f) (lambda (x) (average (f x) x)))

(define (square-root x) (fixedsolvedebug (averagedamp (lambda (y) (/ x y))) 1.0))
(define (cube-root x) (fixedsolvedebug (averagedamp (lambda (y) (/ x (* y y)))) 1.0)) 


(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 1) (compose (repeated f (- n 1)) f) f))

(define (rootfixed x n) (lambda (y) (/ x (power y (- n 1)))))

(define (averagedamps n) (repeated averagedamp n))

(define (nth-root x n a) (fixedsolve ((averagedamps a)(rootfixed x n)) 1.0))


;(define rad 100)
;(define pow (random 1000))

;(power (nth-root rad pow (/(log pow) (log 2))) pow)
;pow

(define (nthroot100 n) (nth-root 100 n (/(log n) (log 2))))
(define (nthpowernthroot100 n) (power (nthroot100 n) n))

(define a (list 1 2 3 4 5 6 7 8 9 10))
a

(define rands (map (lambda (x) (random 1000000)) a))
rands

(map nthroot100 rands)
(map nthpowernthroot100 rands)



;average-damping 
; pow = 2 a = 1
; pow = 3 a = 2
; pow = 4 a = 2
; pow = 5 a = 2
; pow = 6 a = 2
; pow = 7 a = 3

; pow = 16 a = 4
; pow = 32 a = 5













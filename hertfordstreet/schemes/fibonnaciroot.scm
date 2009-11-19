;Fibonnaci, (Leonardo of Pisa), calculated that the root of x^3 + 2x^2 + 10x = 20
;was the babylonian fraction 1;22,7,42,33,4,40
;(despite telling everyone else to use the decimal system in liber abaci)


;(require (lib "trace.ss"))

(define (baby l)
  (cond
    ((null? l) 0)
    (else (+ (car l) (/(baby (cdr l)) 60)))))

;(baby '(1))

(define fibonnacis-root (baby '(1 22 7 42 33 4 40)))

(define (poly l x)
  (cond
    ((null? (cdr l)) (car l))
    ((+ (car l) (* x (poly (cdr l) x))))))

;(trace poly)
;(poly '(1) 2)
;(poly '(1 1) 2)
;(poly '(1 10 100) 2)
;(poly '(1 10 100 1000) 3)

(poly '(0 10 2 1) fibonnacis-root)


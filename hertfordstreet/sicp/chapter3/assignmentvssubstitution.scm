(define (make-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define w (make-withdraw 25))

(w 20)
(w 10)


(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define d (make-decrementer 25))
(d 20)
(d 10)

;analysis of make-decrementer using substitution
;this model breaks down for make-withdraw
((make-decrementer 25) 20)
((lambda (amount) (- 25 amount)) 20)
(- 25 20)
5






(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))(count-leaves (cdr x))))))

;(count-leaves x)
;
;(list x x)
;(count-leaves (list x x))
;(length (list x x))
;
(define a '(1 3 (5 7) 9))
(define b '((7)))
(define c '(1 (2 (3 (4 (5 (6 7)))))))
;a
;b
;c

a
(cdr a)
(cddr a)
(caddr a)
(cdaddr a)
(car (cdaddr a))
(car (cdr (car (cdr (cdr a)))))

b
(car b)
(caar b)

c
(cdr c)
(cadr c)
(cadadr c)
(cadadr (cadadr c))
(cadr (cadadr (cadadr c)))
(cadadr (cadadr (cadadr c)))


;(caddddddr a)
;(caddr(cddddr a))



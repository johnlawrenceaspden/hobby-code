(define a (list 1 2))
(define b (list 3 4))
(define l (list a b))
(define q (list 1 2 3 4))
(define (atom? x) (not (pair? x)))

(define (reverse l)
  (if (null? l) l (append (reverse (cdr l))(list (car l)))))

q
(reverse q)
l 
(reverse l)

(define (deep-reverse l)
  (cond ((atom? l) l)
        (else (append (deep-reverse (cdr l))(list (deep-reverse (car l)))))))

(deep-reverse l)


(define (iter-reverse l)
  (define (reverse-helper a b)
    (if (null? a) b
        (reverse-helper (cdr a)(cons (car a) b))))
  (reverse-helper l ()))

(iter-reverse l)

(define (fringe l)
  (cond ((null? l) ())
        ((atom? l) (list l))
        (else (append (fringe (car l)) (fringe (cdr l))))))

(list l l q)
(fringe (list l l q))
   
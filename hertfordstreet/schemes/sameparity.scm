(define (filter inputlist condition?)
  (if (null? inputlist) ()
  (if (condition? (car inputlist))
      (cons (car inputlist) (filter (cdr inputlist) condition?))
      (filter (cdr inputlist) condition?))))

(define (equivparity a)
  (lambda (x) (= (remainder x 2) (remainder a 2))))

(define (same-parity . inputlist)
  (filter inputlist (equivparity (car inputlist))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 1)
(same-parity 2 3 4 5 6 7 8 9)








(define (duplicate a)
  (if (null? a) ()
      (cons (car a) (duplicate (cdr a)))))

(define (iterdup a)
  (define (loop a b)
    (if (null? a) 
        b 
        (loop (cdr a)(cons (car a) b))))
  (loop a ())) 

(duplicate (list 1 2 3 4 5))
(iterdup (list 1 2 3 4 5))


(duplicate (list 1 2 3 4 5))
(if (null? (list 1 2 3 4 5)) () (cons (car (list 1 2 3 4 5)) (duplicate(cdr (list 1 2 3 4 5)))))
(cons (car (list 1 2 3 4 5)) (duplicate(cdr (list 1 2 3 4 5))))
(cons 1 (duplicate (list 2 3 4 5)))
(cons 1 (cons 2 (duplicate (list 3 4 5))))
(cons 1 (cons 2 (cons 3 (duplicate (list 4 5)))))
(cons 1 (cons 2 (cons 3 (cons 4 (duplicate (list 5))))))
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 ())))))
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 ())))))
(cons 1 (cons 2 (cons 3 (cons 4 (list 5)))))
(cons 1 (cons 2 (cons 3 (list 4 5))))
(cons 1 (cons 2 (list 3 4 5)))
;...
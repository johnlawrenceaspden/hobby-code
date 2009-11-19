(define a (cons 1 
                (cons (cons 8 
                            (cons 9 
                                  (cons 3 
                                        (cons 4 
                                              ()))))
                      (cons 3
                            (cons 4 ())))))

(define b(list 1
               2
               (list 3 4 6 7 
                     (list 'list 3 4 7)
                     (list 5 6 7 8 9))
               4))

(define odds (list 1 3 5 7))
(define squares (list 1 4 9 16))

(define (list-ref items n)
  (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))

(define (length list)
  (if (null? list) 0 (+ 1 (length (cdr list)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2)) ))

(define (last-pair a)
  (if (null? (cdr a)) (car a) (last-pair (cdr a))))

(define (reverse a)
  (if (null? a) a (append (reverse (cdr a)) (cons (car a) ()))))


odds
(reverse odds)




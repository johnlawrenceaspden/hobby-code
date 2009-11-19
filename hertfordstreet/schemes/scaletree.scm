(define tree          '(((1 2) (3 4) 5 6 (((7)(8 9)) 10 11 12   ))13) )
(define semieviltree  '(((1 2) (3 4) 5 6 (((7)(8 9)) 10 11 12 ()))13) )
(define eviltree      '(((1 2) (3 4) 5 6 (((7)(8 9)) a  11 12 ()))13) )

(define l eviltree)
(caaar l)
(cadaar l)
(caadar l)
(car (cdadar l))
(caddar l)
(car (cdddar l))
(caaaar (cddddr (car l)))
(caadar (cadddr (cdar l)))
(cadadr (caaddr (cddar l)))
(cadadr (cdddar l))
(caddar (cddddr (car l)))
(cadr (cddar (cddddr (car l))))
(cadddr (cdaddr (cddar l)))
(cadr l)

(define (handscale-tree factor tree)
  (cond ((null? tree) () )
        ((number? tree) (* factor tree))
        ((pair? tree) (cons (handscale-tree factor (car tree))(handscale-tree factor (cdr tree))))))

(define (map-scale-tree factor tree)
  (map 
   (lambda (x)
     (cond ((list? x) (map-scale-tree factor x))
         ((number? x)(* factor x))))
   tree))

(define (square x) (* x x))
(define (square-tree tree) (treemap square tree))

(define ((scale a) x) (* a x) )
(define (scale-tree a tree) (treemap (scale a) tree))

(define (handcube-tree tree)
  (cond ((null? tree) () )
        ((number? tree) (* tree tree tree))
        ((pair? tree) (cons (handcube-tree  (car tree))(handcube-tree  (cdr tree))))))

(define (handtreemap f tree)
  (cond ((null? tree) ())
        ((number? tree) (f tree))
        ((pair? tree) (cons (handtreemap f (car tree))(handtreemap f (cdr tree))))))

(define (treemap f tree)
  (map
   (lambda (x)
     (cond ((list? x) (treemap f x))
           ((number? x) (f x))))
   tree))

eviltree
(handscale-tree 10 eviltree)
(scale-tree 100 eviltree)
(square-tree eviltree)
(handcube-tree eviltree)
(handtreemap square eviltree)
(treemap (scale 33) eviltree)







(define (mapper f l)
  (if (null? l) ()
      (cons (f (car l)) (mapper f (cdr l)))))


(define (square x) (* x x))
(define a (list 1 2 3 4 5)) 
(map square a)
(mapper square a)

(define (scale-list l f)
  (map (lambda (x) (* f x)) l))

(scale-list a 3)

(define (square-list items)
  (if (null? items) ()
      (cons (square (car items)) (square-list (cdr items)))))

(square-list a)

(define (square-lister items) (mapper square items))

(square-lister a)


;iterative versions do not work so well!

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items ()))

(square-list a)


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons answer (square (car things))
                    ))))
  (iter items ()))

(square-list a)

(for-each (lambda (x) (display x)) a)
(newline)

(define (for-each f l) (map f l))

(for-each (lambda (x) (display x)) a)
(newline)

(define (for-each f l)
  (if (null? l) 0
  (begin (f (car l))
  (for-each f (cdr l)))))

(for-each (lambda (x) (display x)) a)



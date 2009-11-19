(load "johnlib.scm")

(define (sumlist l) (accumulate + 0 l))
(define (complexity n) (sumlist(positionpattern n)))
(define (complexityratio n) (/ (complexity n) (complexity (+ n 1))))
(define (printsuccessivevalues function a b) (for-each (lambda (n) (begin (display (function n))(newline))) (enumerate-interval a b)))
(define (positions k n) (length (queen-cols k n)))
(define (positionpattern  n) (map (lambda (k) (positions k n)) (enumerate-interval 1 n)))


(define empty-board ())
(define (adjoin-position x l)
  (cons x l))

(define (queens f board-size)
  (queen-cols f board-size board-size))

(define (safe? l)
  (and (safe-horizontal? l)
       (safe-upperdiagonal? l)
       (safe-lowerdiagonal? l)))

(define (safe-horizontal? l) (notin? (car l) (cdr l)))

(define (safe-lowerdiagonal? l) (notcheckinglower? (car l)(cdr l)))
(define (safe-upperdiagonal? l) (notcheckingupper? (car l)(cdr l)))

(define (notcheckingupper? a l)
  (if (null? l) #t
      (and (not (= (- a 1) (car l)))  (notcheckingupper? (- a 1) (cdr l)))))

(define (notcheckinglower? a l)
  (if (null? l) #t
      (and (not (= (+ a 1) (car l)))  (notcheckinglower? (+ a 1) (cdr l)))))      

(define (notin? a l)
  (if (null? l) #t (and (not(= a (car l))) (notin? a (cdr l)))))




(define (addqueenandprune f l board-size)
  (filter
   (lambda (positions) (safe? positions))
   (f l board-size)))

(define (queen-cols f k board-size)  
  (if (= k 0) (list empty-board)
      (addqueenandprune f (queen-cols  f (- k 1) board-size) board-size )))


(define ((double f) a b) (f (f a b) b))

;(addqueen '(()) 4)
;((double addqueen) '(()) 4)
;((double (double addqueen)) '(()) 4)

(define (addqueen l board-size) 
  (flatmap
   (lambda (rest-of-queens)
     (map (lambda (new-row)
            (adjoin-position new-row rest-of-queens))
          (enumerate-interval 1 board-size)))
   l))

(define (louisaddqueen l board-size) 
  (flatmap 
   (lambda (new-row)
     (map
      (lambda (rest-of-queens)        
        (adjoin-position new-row rest-of-queens))
      l))
   (enumerate-interval 1 board-size)))

(length(queens addqueen 9))
(length(queens louisaddqueen 9))







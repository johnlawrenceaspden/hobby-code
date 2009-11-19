(load "johnlib.scm")

(define empty-board ())
(define (adjoin-position x n l)
  (cons x l))

(define (queen-cols k board-size)  
  (if (= k 0)
      (list empty-board)
      (filter
       (lambda (positions) (safe? k positions))
       (flatmap
        (lambda (rest-of-queens)
          (map (lambda (new-row)
                 (adjoin-position new-row k rest-of-queens))
               (enumerate-interval 1 board-size)))
        (queen-cols (- k 1) board-size)))))

(define (queens board-size)
  (queen-cols board-size board-size))

(define (safe? k l)
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



(define (positions k n) (length (queen-cols k n)))


(define positionpattern (lambda (n) (map (lambda (k) (positions k n)) (enumerate-interval 1 n))))
(define (sumlist l) (accumulate + 0 l))
(define (complexity n) (sumlist(positionpattern n)))
(define (complexityratio n) (/ (complexity n) (expt 3 (- n 1))))

(define (printsuccessivevalues function a b) (for-each (lambda (n) (begin (display (function n))(newline))) (enumerate-interval a b)))

(queens 8)





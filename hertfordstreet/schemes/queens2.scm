(load "johnlib.scm")
;(addqueen 1 ((1 2) (3 4))) -> ((1 1 2) (1 3 4)))

(define (addqueen row positions)
  (map (lambda(x) (cons row x)) positions))

(define (addqueens board-size positions)
  (flatmap (lambda (x) (addqueen x positions)) (enumerate-interval 1 board-size)))

(define (queencols k board-size)
  (if (= k 0) '(())
           (filter safe? (addqueens board-size (queencols (- k 1) board-size)))))

(define (queens board-size) (queencols board-size board-size))

;(safe? ( a b c d )) a <> b a+1<> b a-1 <> b a<> c a+2 <>c a-2 <>c etc

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



(queens 4)

(safe? '(2 4 1 3))
(safe? '(3 1 4 2))

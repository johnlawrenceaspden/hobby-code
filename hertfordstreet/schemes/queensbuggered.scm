(load "johnlib.scm")
(define (enumerate-interval low high)
  (if (> low high) '() (cons low (enumerate-interval (+ low 1) high))))

(define empty-board '())
(define (adjoin-position x l)
  (cons x l))



(define (queens board-size)
  (queen-cols board-size board-size))

(define (louisqueens board-size)
  (louisqueen-cols board-size board-size))

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



(define (positions k n) (length (queen-cols k n)))

(define (fold-right op initial sequence) ;(fold-right cons () (a b c)) is (cons a(cons b(cons c ())))
  (if (null? sequence) initial (op (car sequence) (fold-right op initial (cdr sequence)))))
(define positionpattern (lambda (n) (map (lambda (k) (positions k n)) (enumerate-interval 1 n))))
(define (sumlist l) (fold-right + 0 l))
(define (complexity n) (sumlist(positionpattern n)))
(define (complexityratio n) (/ (complexity n) (expt 3 (- n 1))))

(define (printsuccessivevalues function a b) (for-each (lambda (n) (begin (display (function n))(newline))) (enumerate-interval a b)))

(define (strip l)
  (filter (lambda (positions) (safe? positions)) l))

(define (flatmap proc seq) (fold-right append '() (map proc seq)))

(define (queen-cols k board-size)  
  (if (= k 0)
      (list empty-board)
      (strip
       
       (flatmap
        (lambda (rest-of-queens)
          (map (lambda (new-row)
                 (adjoin-position new-row rest-of-queens))
               (enumerate-interval 1 board-size)))
        (queen-cols (- k 1) board-size))
       
       
       )))

(define (louisqueen-cols k board-size)  
  (if (= k 0)
      (list empty-board)
      (strip
       
       (flatmap (lambda (new-row)
                  (map
                   (lambda (rest-of-queens)
                     (adjoin-position new-row rest-of-queens))
                   (louisqueen-cols (- k 1) board-size) ))
                (enumerate-interval 1 board-size))
       
       
       )))


(define (displayresults n cpu louiscpu )
  (printf "~s: ~s ~s ~s           " n louiscpu cpu (floor (/ louiscpu cpu)) )
  (display (sumlist (map (lambda (x) (expt n x))(enumerate-interval 0 n))))
  (display (map (lambda (x) (expt n x))(enumerate-interval 0 n)))
  (newline))

;profiling on
;(displayresults 3 2 8)
;(displayresults 4 7 73)
;(displayresults 5 25 948)
;(displayresults 6 104 17680)
;(displayresults 7 444 314667)
;

;debugging and profiling off
;(displayresults 4 1 5)
;(displayresults 5 2 59)
;(displayresults 6 7 924)
;(displayresults 7 33  24248)
;(displayresults 8 159 515568)
;(displayresults 9 6173 0)

;;syntactic test coverage on
;(displayresults 3 3  17)
;(displayresults 4 16 116)
;(displayresults 5 53  1437)
;(displayresults 6 194 28102)





(for-each (lambda (n)
            (newline)
            (display n)
            (time (queens n))
            (time (louisqueens n))
            ) (list 3 4 5 6 7 8 9))



;(queen-cols 1 3)
;(queen-cols 2 3) 
;
;(strip(flatmap 
; (lambda (a) 
;   (map 
;    (lambda (b) 
;      (list a b))
;    '(1 2 3)))
;   '(1 2 3 4)))
;
;(define (doublemap f l1 l2)
;  (flatmap (lambda(a) (map (lambda(b) (f a b)) l1)) l2))
;
;(define (doublerevmap f l1 l2)
;  (doublemap (lambda (a b) (f b a)) l2 l1))
;
;(doublemap cons '(1 2 3) '(a b c d))
;
;(doublemap cons '( a b c d) (list 1 2 3))
;
;
;(doublerevmap cons '(1 2 3) '(a b c d))
;
;(doublerevmap cons '( a b c d) (list 1 2 3))



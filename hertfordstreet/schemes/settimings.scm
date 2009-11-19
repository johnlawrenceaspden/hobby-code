(require (lib "trace.ss"))
(load "johnlib.scm")

(define (randomlist n) (map (lambda (x) (random n)) (enumerate-interval 0 n)))

(define (randomset n) (list->set (randomlist n)))

(define (dotests n)
  (define a (time (randomset n)))
  (define b (time (randomset n)))
  (define c (randomlist 1000))
  
  (time (begin (union a b) #t))
  (time (begin (intersection a b) #t))
  (time (map (lambda (x) (element-of-set? x a)) c))
  (time (for-each (lambda (x) (adjoin-set x a)) c)))

(define (settest file)
  (newline)(display file)(newline)(load file)(newline)
  (dotests 10000))



;(settest "setsasunorderednoduplicatelists.scm")
;(settest "setsasorderedlists.scm")
(settest "setsastrees.scm")
;(settest "setsaslistswithduplicates.scm")




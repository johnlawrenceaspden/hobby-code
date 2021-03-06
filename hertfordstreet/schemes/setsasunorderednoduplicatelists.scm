(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) set (cons x set)))

(define (intersection set1 set2)
  (cond ((null? set1) '())
        ((element-of-set? (car set1) set2) (adjoin-set (car set1) (intersection (cdr set1) set2)))
        (else (intersection (cdr set1) set2))))

(define (union set1 set2)
   (cond ((null? set1) set2)
        (else (adjoin-set (car set1) (union (cdr set1) set2)))))

;tests
(define (test expression value)
  (let ((answer (eval expression)))
    (if (equal? answer value)
        (begin (display "pass ")
               #t)
        (begin
          (display "test failed: ") (newline)
          (display "expression :")(display expression)
          (display " was ") (display answer)
          (display " <> ") (display value)(newline)
          #f))))

(define (list->set l)
  (if (null? l) 
      '()
      (adjoin-set (car l) (list->set (cdr l)))))

(define testset1 (list->set '(a b c 5 2 b  2)))
(define testset2 (list->set '(a b hippo zebra 5 3)))
(define emptyset (list->set '()))
(define b 3)
(define d 2)
(if (and
     (test '(element-of-set? 'a testset1) #t)
     (test '(element-of-set? 'd testset1) #f)
     (test '(element-of-set? d testset1) #t)
     (test '(element-of-set? 2 testset1) #t)
     (test '(element-of-set? 'b testset1) #t)
     (test '(element-of-set? b testset1) #f)
     (test '(intersection testset1 testset2) (list->set '(a 5 b)))
     (test '(intersection emptyset testset2) emptyset)
     (test '(intersection testset1 emptyset) emptyset)
     (test '(intersection emptyset emptyset) emptyset)
     (test '(union emptyset emptyset) emptyset)
     (test '(union emptyset testset1) testset1)
     (test '(union testset1 emptyset) testset1)
     (test '(union testset1 testset2) (list->set '(c 2 a b hippo zebra 5 3)))
     
     )
    (begin (display "self-test successful") (newline))
    (begin (display "self-test failed") (newline)))
    
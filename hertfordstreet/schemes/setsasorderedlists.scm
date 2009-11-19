(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((< x (car set)) #f)
        ((= x (car set)) #t)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


(define (intersection set1 set2)
  (if (or (null? set1)(null? set2)) 
      '()
      (let ((e1 (car set1))
            (e2 (car set2)))
        (cond ((< e1 e2) (intersection (cdr set1) set2))
              ((= e1 e2) (cons e1 (intersection (cdr set1) (cdr set2))))
              ((> e1 e2) (intersection set1 (cdr set2)))))))

(define (union set1 set2)
  (cond ((and (null? set1)(null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((e1 (car set1))
                    (e2 (car set2)))
                (cond ((< e1 e2) (cons e1 (union (cdr set1) set2)))
                      ((= e1 e2) (cons e1 (union (cdr set1) (cdr set2))))
                      ((> e1 e2) (cons e2 (union set1 (cdr set2)))))))))



;tests



(define (list->set l)
  (if (null? l) 
      '()
      (adjoin-set (car l) (list->set (cdr l)))))

(define testset1 (list->set '(10 20 30 5 2 2 2)))
(define testset2 (list->set '(10 2 7 4 5 3)))
(define emptyset (list->set '()))

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



(if (and
     (test '(element-of-set? 8 testset1) #f)
     (test '(element-of-set? 2 testset1) #t)
     (test '(intersection testset1 testset2) (list->set '(10 5 2)))
     (test '(intersection emptyset testset2) emptyset)
     (test '(intersection testset1 emptyset) emptyset)
     (test '(intersection emptyset emptyset) emptyset)
     (test '(union emptyset emptyset) emptyset)
     (test '(union emptyset testset1) testset1)
     (test '(union testset1 emptyset) testset1)
     (test '(union testset1 testset2) (list->set '(2 3 4 5 7 10 20 30)))
     
     )
    (begin (display "self-test successful") (newline))
    (begin (display "self-test failed") (newline)))



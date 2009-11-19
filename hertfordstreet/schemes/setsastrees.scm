(load "binarytrees.scm")

(define (invert function x set)
  (cond ((null? set) #f)
        (( = x (function(entry set))) (entry set))
        (( < x (function(entry set))) (invert function x (left-branch set)))
        (( > x (function(entry set))) (invert function x (right-branch set)))))

;(define (element-of-set? x set)
;  (if (eq? #f (invert (lambda (x) x) x set)) #f #t))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
        ((> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))))

(define (unorderedlist->set l)
  (if (null? l) 
      '()
      (adjoin-set (car l) (unorderedlist->set (cdr l)))))

(define (list->set l)
  (balance (unorderedlist->set l)))

(define (set->list s) (tree->list s))

(define (balance set)
  (list->tree (tree->list set)))


(define (orderedlistunion list1 list2)
  (cond ((and (null? list1)(null? list2)) '())
        ((null? list1) list2)
        ((null? list2) list1)
        (else (let ((e1 (car list1))
                    (e2 (car list2)))
                (cond ((< e1 e2) (cons e1 (orderedlistunion (cdr list1) list2)))
                      ((= e1 e2) (cons e1 (orderedlistunion (cdr list1) (cdr list2))))
                      ((> e1 e2) (cons e2 (orderedlistunion list1 (cdr list2)))))))))

(define (union set1 set2)
  (list->tree (orderedlistunion (tree->list set1) (tree->list set2))))

(define (orderedlistintersection list1 list2)
  (if (or (null? list1)(null? list2)) 
      '()
      (let ((e1 (car list1))
            (e2 (car list2)))
        (cond ((< e1 e2) (orderedlistintersection (cdr list1) list2))
              ((= e1 e2) (cons e1 (orderedlistintersection (cdr list1) (cdr list2))))
              ((> e1 e2) (orderedlistintersection list1 (cdr list2)))))))

(define (intersection set1 set2)
  (list->tree (orderedlistintersection (tree->list set1) (tree->list set2))))

;tests

(define (_test_setsastrees expression value)
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



(define (_dotests_setsastrees)
(if (and

     
     (_test_setsastrees '(element-of-set? 8 testset1) #f)
     (_test_setsastrees '(element-of-set? 2 testset1) #t)
     (_test_setsastrees '(element-of-set? 20 testset1) #t)
     (_test_setsastrees '(invert square 400 testset1 ) 20)
     (_test_setsastrees '(invert square 2 testset1 ) #f)     
     (_test_setsastrees '(invert square 4 testset1 ) 2)     
     (_test_setsastrees '(intersection testset1 testset2) (list->set '(10 5 2)))
     (_test_setsastrees '(intersection emptyset testset2) emptyset)
     (_test_setsastrees '(intersection testset1 emptyset) emptyset)
     (_test_setsastrees '(intersection emptyset emptyset) emptyset)
     (_test_setsastrees '(union emptyset emptyset) emptyset)
     (_test_setsastrees '(union emptyset testset1) testset1)
     (_test_setsastrees '(union testset1 emptyset) testset1)
     (_test_setsastrees '(union testset1 testset2) (list->set '(2 3 4 5 7 10 20 30)))
     (_test_setsastrees '(set->list (list->set '(1 5 6 3 4 2 ))) '(1 2 3 4 5 6)) 
     
     )
    (begin (display "self-test successful") (newline))
    (begin (display "self-test failed") (newline))))

(define testset1 (list->set '(10 20 30 5 2 2 2)))
(define testset2 (list->set '(10 2 7 4 5 3)))
(define emptyset (list->set '()))
(define (square x) (* x x))

(display "setsastrees.scm selftests ")
(_dotests_setsastrees)

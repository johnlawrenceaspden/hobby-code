;definition of binary tree structure
(define (make-tree entry left right) (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))





;tests




(define (_dotests_binarytrees)
  
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
       
       (test '(entry toytree) 'c)
       (test '(entry (right-branch toytree)) 'b)
       (test '(entry (left-branch toytree)) 'a)
       (test '(tree->list toytree) '(a c b))
       (test '(tree->list bigtree) '(a b c d e f g h i j))
       
       )
      (begin (display "self-test successful") (newline))
      (begin (display "self-test failed") (newline))))

(display "binarytrees.scm selftests ")
(define toytree (make-tree 'c (make-tree 'a '() ()) (make-tree 'b '() ())))
(define bigtree (list->tree '(a b c d e f g h i j)))
(_dotests_binarytrees)

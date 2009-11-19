(load "binarytrees.scm")

(define (makerecord key data) (cons key data))
(define (key record) (car record))
(define (data record) (cdr record))

(define emptydatabase '())

(define (adjoin-database x db)
  (cond ((null? db) (make-tree x '() '()))
        ((= (key x) (key (entry db))) (make-tree x (left-branch db) (right-branch db)))
        ((< (key x) (key (entry db))) (make-tree (entry db) (adjoin-database x (left-branch db)) (right-branch db)))
        ((> (key x) (key (entry db))) (make-tree (entry db) (left-branch db) (adjoin-database x (right-branch db))))))


(define (invert function x set)
  (cond ((null? set) #f)
        (( = x (function(entry set))) (entry set))
        (( < x (function(entry set))) (invert function x (left-branch set)))
        (( > x (function(entry set))) (invert function x (right-branch set)))))

(define (get-record k db)
  (invert (lambda (x) (key x)) k db))

(define (get-data k db) (data(get-record k db)))

(define (recordlist->database rl)
  (if (null? rl) emptydatabase (adjoin-database (car rl) (recordlist->database (cdr rl)))))


(define telephonebook1
  (adjoin-database (makerecord 234567 '("git" "evans"))
                   (adjoin-database (makerecord 526562 "john") 
                                    '())))

(define telephonebook2
  (recordlist->database (list
                         (makerecord 01223234567 '("git" "evans"))
                         (makerecord 01223526562 "jhn")
                         (makerecord 0878555444 "sexyfluffies"))))

(define telephonebook3 (adjoin-database(makerecord 01223526562 "john") telephonebook2))

(get-data 0878555444 telephonebook2)
(get-data 01223526562 telephonebook2)
(get-data 78978979202 telephonebook2)

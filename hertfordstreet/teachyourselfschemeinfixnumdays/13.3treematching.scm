(require (lib "trace.ss"))

(define flatten
  (lambda (tree)
    (cond ((null? tree) '())
          ((pair? (car tree))
           (append (flatten (car tree)) 
                   (flatten (cdr tree))))
          (else
           (cons (car tree)
                 (flatten (cdr tree)))))))

;(define same-fringe? 
;  (lambda (tree1 tree2)
;    (equal? (flatten tree1) (flatten tree2))))

(define (same-fringe? tree1 tree2)
  (let loop ((ftree1 (flatten tree1))
             (ftree2 (flatten tree2)))
    (cond ((and (null? ftree1) (null? ftree2)) #t)
          ((or (null? ftree1) (null? ftree2)) #f)
          ((eqv? (car ftree1) (car ftree2))
           (loop (cdr ftree1) (cdr ftree2)))
          (else #f))))




(display "#t:") (same-fringe? '(1 (2 3)) '((1 2) 3) ) 
(display "#f:") (same-fringe? '(1 (2 3)) '(1 (4 3)) )

(define evillist          '((1 () (2 3) (4 5 ((6) 7) (8 9) 10) )))
(define lessevillist      '( () 1 2 3 (4 5) 6 7 ((8 9)) 10))
(define nicelist          '( () 1 2 3 4 5 6 7 8 (9) 10))


(display "#f:")(same-fringe? evillist lessevillist)
(display "#t:")(same-fringe? nicelist lessevillist)


(define (tree->generator tree)
  (let ((caller '*))
    (letrec
        ((generate-leaves
          (lambda ()
            (let loop ((tree tree))
              (cond ((null? tree) 'skip)
                    ((pair? tree)
                     (loop (car tree))
                     (loop (cdr tree)))
                    (else
                     (call/cc
                      (lambda (rest-of-tree)
                        (set! generate-leaves
                              (lambda ()
                                (rest-of-tree 'resume)))
                        (caller tree))))))
            (caller '()))))
      (lambda ()
        (call/cc
         (lambda (k)
           (set! caller k)
           (generate-leaves)))))))

(define evilgenerator (tree->generator evillist))











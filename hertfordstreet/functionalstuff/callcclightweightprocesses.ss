#lang scheme

;;ruminations on the continuations section of "The Scheme Programming Language".
;; http://scheme.com/tspl3/further.html#./further:h4






;nothing to see here, just a standard queue implemented as a list
(define queue '())
(define (qpop)     (begin0 (car queue) (set! queue (cdr queue))))
(define (qpush p)  (set! queue (append queue (list p))))

;;lightweight processes

(define (pause)
  (call/cc (λ(k)
             (qpush (λ() (k #f)))
             ((qpop)))))

(qpush (λ() (let f() (pause) (display "h") (f))))
(qpush (λ() (let f() (pause) (display "e") (f))))
(qpush (λ() (let f() (pause) (display "l") (f))))
(qpush (λ() (let f() (pause) (display "l") (f))))
(qpush (λ() (let f() (pause) (display "o") (f))))
(qpush (λ() (let f() (pause) (display "!") (f))))

((qpop))




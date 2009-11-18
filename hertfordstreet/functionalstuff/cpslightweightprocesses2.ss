#lang scheme

;;ruminations on the continuations section of "The Scheme Programming Language".
;; http://scheme.com/tspl3/further.html#./further:h4

;; lightweight processes, original program with call/cc transformed into a 
;; program in continuation passing style to avoid the need for magic.

;; further tidying up to see if we can expose the essence of the thing.

;nothing to see here, just a standard queue implemented as a list
(define queue '())
(define (qpop)     (begin0 (car queue) (set! queue (cdr queue))))
(define (qpush p)  (set! queue (append queue (list p))))

;;lightweight processes, replacing the call/cc of the original with
;;cps transform

(define (pause cont)
  (qpush (λ() (cont)))
  ((qpop)))

(qpush (λ() (let f() (pause (λ() (display "h") (f))))))
(qpush (λ() (let f() (pause (λ() (display "e") (f))))))
(qpush (λ() (let f() (pause (λ() (display "l") (f))))))
(qpush (λ() (let f() (pause (λ() (display "l") (f))))))
(qpush (λ() (let f() (pause (λ() (display "o") (f))))))
(qpush (λ() (let f() (pause (λ() (display "!") (f))))))
((qpop))










  
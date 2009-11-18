#lang scheme

;;ruminations on the continuations section of "The Scheme Programming Language".
;; http://scheme.com/tspl3/further.html#./further:h4

;;lightweight processes
(define lwp-list '())

(define (lwp thunk)
    (set! lwp-list (append lwp-list (list thunk))))

(define (start)
    (let ((p (car lwp-list)))
      (set! lwp-list (cdr lwp-list))
      (p)))

(define (pause)
  (call/cc (λ(k)
             (lwp (λ() (k #f)))
             (start))))

(lwp (λ() (let f() (pause) (display "h") (f))))
(lwp (λ() (let f() (pause) (display "e") (f))))
(lwp (λ() (let f() (pause) (display "l") (f))))
(lwp (λ() (let f() (pause) (display "l") (f))))
(lwp (λ() (let f() (pause) (display "o") (f))))

(start)




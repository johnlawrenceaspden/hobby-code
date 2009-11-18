#lang scheme

;;ruminations on the continuations section of "The Scheme Programming Language".
;; http://scheme.com/tspl3/further.html#./further:h4

;; lightweight processes, original program with call/cc transformed into a 
;; program in continuation passing style to avoid the need for magic.

;;this was my first cps transform, just done blindly to all functions.
;;The program gains in clarity when most of this is reversed, leaving only the
;;transformation needed for pause. See cpslightweightprocesses2.ss

;nothing to see here, just a standard queue implemented as a list
(define queue '())
(define (qpop)     (begin0 (car queue) (set! queue (cdr queue))))
(define (qpush p)  (set! queue (append queue (list p))))

(define (lwp thunk cont)
  (qpush thunk)
  (cont))

(define (start)
  ((qpop))) 

(define (pause cont)
  (lwp (λ() (cont)) (λ() (start))))

(lwp 
 (λ() (let f() (pause (λ() (display "h") (f)))))
 (λ() (lwp 
       (λ() (let f() (pause (λ() (display "e") (f)))))
       (λ() (lwp 
             (λ() (let f() (pause (λ() (display "l") (f)))))
             (λ() (lwp 
                   (λ() (let f() (pause (λ() (display "l") (f)))))
                   (λ() (lwp 
                         (λ() (let f() (pause (λ() (display "o") (f)))))
                         (λ() (lwp 
                               (λ() (let f() (pause (λ() (display "!") (f)))))
                               (λ() (start))
             )))))))))))











  
(require (lib "trace.ss"))

(define prim-* *)

(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))

(trace fact)

(fact 10)

(set! * (lambda (a b) (list '* a b)))
(fact 10)
(set! * prim-*)
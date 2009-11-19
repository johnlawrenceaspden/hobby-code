#lang scheme

;here is a definition of a function in terms of itself.

(define expt (λ ( x n )
  (cond ((= n 0) 1)
        (else
         (* x (expt x (- n 1)))))))

;it can be viewed as a recursion equation. expt is the function which when substituted in gives back expt.

;lets define a function whose fixed point might be expt

(define F (λ (g)
            (λ (x n)
              (cond ((= n 0) 1)
                    (else
                     (* x (g x (- n 1))))))))

(define (expt0 x n) 0)

; and behold....

(expt0 5 3)
((F expt0) 5 3)
((F (F expt0)) 5 3)
((F (F (F expt0))) 5 3)
((F (F (F (F expt0)))) 5 3)
((F (F (F (F (F expt0))))) 5 3)

(provide F)
(provide expt0)
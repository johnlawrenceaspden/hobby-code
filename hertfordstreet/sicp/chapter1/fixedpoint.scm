(define (findfixedpointhelper a1 a2 func tolerance)
  (printf "~a~n" a2)
  (if ( < (abs ( - a1 a2)) tolerance)
      a2
      (findfixedpointhelper a2 (func a2) func tolerance)))

(define (findfixedpoint f initial tolerance)
  (printf "~a~n" initial)
  (findfixedpointhelper initial (f initial) f tolerance))

(define (huronsqrtfn x a)
  (/ (+ x (/ a x) ) 2))
  
(define (sqrt a)
  (findfixedpoint (lambda (x) (huronsqrtfn x a))1.0 0.0001))
  
(sqrt 100)
(sqrt 1000)
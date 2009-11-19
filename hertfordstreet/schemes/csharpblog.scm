(define fib void)

(set! fib 
      (lambda (n)
        (if (< n 2)
            n
            (+ (fib (- n 1)) 
               (fib (- n 2))))))

(define fibcopy fib)

(fib 6)
(fibcopy 6)

(set! fib
      (lambda (n) 
        (* n 2)))

(fib 6)
(fibcopy 6)



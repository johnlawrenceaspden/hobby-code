(require (lib "trace.ss"))

(define (Hello)
  (display "Hello")
  (newline))



(define (hash-table-print a)
  (hash-table-for-each a (lambda (k v) (display (format "k=~a, v=~a\n" k v))))
  )


(define (memoize f)
  (define cache (make-hash-table 'equal))
  (lambda x
    (if (equal? x '(dump)) (hash-table-print cache)
        (let ((cachevalue (hash-table-get cache x 'unknown)))
          (cond ((equal? cachevalue 'unknown) (begin
                                                (display (format "cache miss ~a\n" x))
                                                (let ((newvalue (apply f x)))
                                                  (display (format "storing ~a=~a\n" x newvalue))
                                                  (hash-table-put! cache x newvalue)            
                                                  newvalue)))
                (else (begin
                        (display (format "cache hit ~a=~a\n" x cachevalue))
                        cachevalue)))))))

(define (fib n)
  (case n 
    ((0) 0)
    ((1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))



(define (memoize-demo)
  (let
      ((Hello (memoize Hello))
       (* (memoize *))
       (fib (memoize fib)))
    (* 7 8)
    (* 7 8)
    (* 'dump)
    
    (fib 5)
    (fib 'dump)
    (Hello)
    (Hello)
    (Hello 'dump))
  (set! fib (memoize fib))
  (fib 5)
  (fib 'dump))

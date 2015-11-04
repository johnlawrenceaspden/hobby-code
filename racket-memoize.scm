#lang racket

(define (memoize f)
  (let ((table (make-hash)))
    (lambda args
      ;; Look up the arguments.
      ;; If they're present, just give back the stored result.
      ;; If they're not present, calculate and store the result.
      ;; Note that the calculation will not be expensive as long
      ;; as f uses this memoized version for its recursive call,
      ;; which is the natural way to write it!
      (dict-ref! table args
                 (lambda ()
                   (apply f args))))))



(define fib 
  (memoize
    (lambda (n)
      (if (< n 1) 1
        (+ (fib (- n 1)) (fib (- n 2)))))))

;;(define fib 
;;  (lambda (n)
;;    (if (< n 1) 1
;;      (+ (fib (- n 1)) (fib (- n 2))))))

;;(define (fib n)
;;  (if (< n 1) 1
;;      (+ (fib (- n 1)) (fib (- n 2)))))


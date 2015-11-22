#lang racket

(define (factorial n)
  (if (< n 2) 1
      (* n (factorial (- n 1)))))


(define (ifact-h n total)
  (if (= n 0) total
      (ifact-h (- n 1) (* n total))))

(define (ifact n) (ifact-h n 1))

(ifact 10)
(factorial 10)

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

(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define mfib 
   (memoize (lambda (n)
      (if (< n 1) 1
        (+ (mfib (- n 1)) (mfib (- n 2)))))))



(define (fact-odd n)
  (if (= 0 (remainder (factorial n) 2)) "even" "odd"))




;(time (even? (ifact 5000)))
;(time (even? (factorial 5000)))
;(time (even? (ifact 10000)))
;(time (even? (factorial 10000)))
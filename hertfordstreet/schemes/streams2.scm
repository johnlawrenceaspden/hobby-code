(require (lib "trace.ss"))

(define-macro (my-delay a)
  `(lambda () ,a))

(define (my-force a) (a))

(define-macro (cons-stream a b)
  `(cons ,a (my-delay ,b)))

(define (head a)
  (car a))

(define (tail a)
  (my-force (cdr a)))

(define empty-stream '())

(define (empty-stream? a)
  (null? a))

(define (enumerate-interval a b)
  (if (<= a b) 
      (cons-stream a (enumerate-interval (+ a 1) b))
      empty-stream))

(define (stream-filter filter? s)
  (if (empty-stream? s) empty-stream
      (let ((a (head s)))
        (if (filter? a) (cons-stream a (stream-filter filter? (tail s))) (stream-filter filter? (tail s))))))

(define (display-stream max s)
  (if (= max 0) (newline)
      (if (not (empty-stream? s))
          (begin
            (printf "~a, " (head s))
            
            (display-stream (- max 1) (tail s))))))

(define (spool-stream max s)
  (if (= max 0) (newline)
      (if (not (empty-stream? s))
          (begin
            (printf "~a\n" (head s))
            
            (spool-stream (- max 1) (tail s))))))

(define (sum-stream a b)
  (cond ((or (empty-stream? a) (empty-stream? b)) empty-stream)
        (else (cons-stream (+ (head a) (head b)) (sum-stream (tail a) (tail b))))))

(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (nth-stream (- n 1) (tail s))))

  
(define (square n) (* n n))
(define (divides? a b) (= 0 (remainder b a)))

;try all divisors up to square root

(define (smallest-divisor n)
  (test-divisor n 2))

(define (test-divisor n i)
  (cond ((> (square i) n) n)
        ((divides? i n) i)
        (else (test-divisor n (+ i 1)))))

(define (prime? n)
  (= (smallest-divisor n) n)) 





(define q (sum-stream (cons-stream 1 q) (cons-stream 0 q)))

(define p (cons-stream 0 (cons-stream 1 (sum-stream p (tail p)))))  

(define (stream-filter pred? s)
  (if (empty-stream? s) empty-stream
      (let ((el (head s)))
        (if (pred? el) (cons-stream el (stream-filter pred? (tail s)))
            (stream-filter pred? (tail s))))))
  
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define integers (integers-from 1)) 
  
(define primes (stream-filter prime? integers)) 
  
;(trace prime? )
;(display-stream 10 p)
;(display-stream 10 q)
(display-stream 10 primes)

(define (sieve s)
  (cons-stream
   (head s)
   (sieve (filter
           (lambda (x) (not (divides s x)))
           (tail s)))))


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
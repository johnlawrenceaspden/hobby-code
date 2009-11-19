(require (lib "trace.ss"))

(define-macro (my-delay a)
  `(lambda () ,a))

(define (my-force a) (a))

(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))

(define (head a)
  (car a))

(define (tail a)
  (force (cdr a)))

(define the-empty-stream '())

(define (empty-stream? a)
  (null? a))

(define (stream-filter filter? s)
  (if (empty-stream? s) the-empty-stream
      (let ((a (head s)))
        (if (filter? a) (cons-stream a (stream-filter filter? (tail s))) 
            (stream-filter filter? (tail s))))))

;(define (stream-map f s)
;  (if (empty-stream? s) the-empty-stream
;      (cons-stream (f (head s)) (stream-map f (tail s)))))

(define (stream-map proc . argstreams)
  (if (empty-stream? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map head argstreams))
                   (apply stream-map (cons proc (map tail argstreams))))))

(define (list->stream l)
  (if (null? l) the-empty-stream
      (cons-stream (car l) (list->stream (cdr l)))))

(define (display-stream max s)
  (if (= max 0) (newline)
      (if (not (empty-stream? s))
          (begin
            (display (head s))
            (display ", ")
            (display-stream (- max 1) (tail s))))))

(define (add-streams a b)
  (cond ((or (empty-stream? a) (empty-stream? b)) the-empty-stream)
        (else (cons-stream (+ (head a) (head b)) (add-streams (tail a) (tail b))))))

(define (scale-stream c s)
  (stream-map (lambda (x) (* c x)) s))

(define (stream-ref s n)
  (if (= n 0)
      (head s)
      (stream-ref (tail s) (- n 1))))

(define (stream-enumerate-interval a b)
  (if (<= a b) 
      (cons-stream a (stream-enumerate-interval (+ a 1) b))
      the-empty-stream))

(define (integers-from n) (cons-stream n (integers-from (+ n 1))))

(define integers (integers-from 1))

(define q (add-streams (cons-stream 1 q) (cons-stream 0 q)))

(define p (cons-stream 0 (cons-stream 1 (add-streams p (tail p)))))  

(define ones (cons-stream 1 ones))  

(define integers
  (cons-stream 1
               (add-streams integers ones)))

(define (square n) (* n n))

(define (divides? a b) (= 0 (remainder b a)))

;definition of primality by trying all possible divisors up to square root
(define (prime? n) 
  (define (smallest-divisor n)
    (test-divisor n 2))
  (define (test-divisor n i)
    (cond ((> (square i) n) n)
          ((divides? i n) i)
          (else (test-divisor n (+ i 1)))))
  (= (smallest-divisor n) n))

(define primes-using-prime (stream-filter prime? (integers-from 2)))

;clever definition of the primes by sieving. Why on earth is it so much slower than the one above?
(define (sieve s)
  (cons-stream (head s)
               (sieve
                (stream-filter 
                 (lambda (x) (not (divides? (head s) x)))
                 (tail s)))))

(define primes-using-sieve (sieve (integers-from 2)))


(define (integral-old s initial-value dx)
  (cons-stream initial-value
               (add-streams (integral-old s initial-value dx) (scale-stream dx s))))

(define (integral s initial-value dx)
  (define int
    (cons-stream 
     initial-value
     (add-streams 
      (scale-stream dx s)
      int)))
  int)


(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams fibs (tail fibs)))))


(define y (cons-stream 1 (integral (stream-map square (cons-stream 1 y)) 0.1 0.01)))

(define-macro (display-line a)
  `(begin (display ,a) (newline)))

(define (show x)
  (display-line x)
  x)

;(begin (define x (stream-map show (stream-enumerate-interval 0 10)))
;       (stream-ref x 5)
;       (stream-ref x 7)) ; try with and without memoization of delay

;(begin 
;  (define sum 0)
;  
;  (define (accum x)
;    (set! sum (+ x sum))
;    sum)
;  
;  (display-line sum)
;  (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;  (display-line sum)
;  (define y (stream-filter even? seq))
;  (display-line sum)
;  (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
;  (display-line sum)
;  (stream-ref y 7)
;  (display-line sum)
;  (display-stream 10 z)
;  (display-line sum)) ; assignment and streams really don't mix. try that without the memoization!


;(stream-ref primes-using-sieve 100) ;5853 calls to divide
;(stream-ref primes-using-prime 100) ;2368


;(stream-ref primes-using-sieve 1000) ; 517416 calls to divide
;(stream-ref primes-using-prime 1000) ; 85810

































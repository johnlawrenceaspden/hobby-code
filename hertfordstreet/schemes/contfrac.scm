(define (cont-frac n d k)
  (define (iter i)
    (if (< i k) 
        (/ (n i) (+ (d i) (iter (+ 1 i))))
        (/ (n k) (d k))
        ))  
  (iter 1))

(define (const n) 1)

;(cont-frac const const 3)

(define (cont-frac n d k)
  (define (iter k total)
    ( if (> k 0)
         (iter (- k 1) (/ (n k) (+ (d k) total)))
         total))
  (iter (- k 1)(/ (n k)(d k))))

;(cont-frac const const 3)


(define (invgolden k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(define (approxmetrics exact fn n)
  (define (err x) (-(fn x) exact))
  (define (crate x) (/(err x)(err (+ 1 x))))
  (define (iter k)
    (printf "~s:~s:\t~s--------~s~%" k (fn k) (err k) (crate k))
    (if (< k n) (iter (+ k 1))))
  (printf "~s~%" exact)
  (iter 1))

;(approxmetrics (/ 1 (/(+(sqrt 5) 1) 2)) invgolden 13)

(define (power n m)
  (if (= m 0) 1 (* n (power n (- m 1)))))

(define (eulerd n)
  (if (= 0 (remainder (- n 2) 3))
      (+ 2 (* 2 (/ (- n 2) 3)))
      1))

(define (e-2 k)
  (cont-frac (lambda (i) 1)
             eulerd 
             k))

(define (tann n x) 
  (if (= n 1) x (- (* x x))))

(define (tan-cf x k)
  (cont-frac
   (lambda (i) (tann i x))
   (lambda (i) (+ 1 (* 2 (- i 1))))
   k))

(approxmetrics (tan 0.1)
               (lambda (k) (tan-cf (/ 1 10) k))
               13)

  










(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))

(define (divides? a b) (= 0 (remainder b a)))

(define (smallest-divisor n)
(define (seek i)
  (cond ((> (square i) n) n)
        ((divides? i n) i)
        (else (seek (+ i 1)))))  
  (seek 2))

(define (prime? n)
  (if (= n 1)
      #f
      (= (smallest-divisor n) n) ))





(define (cube x) (* x x x ))
(define (square x) (* x x))
(define (tesseract x) (* x x x x))

(define (intsin x) (- (cos x)))
(define (intcube x) (/ (* x x x x) 4))
(define (inttesseract x) (/ (* x x x x x) 5))

(define (integral f) 
  (cond ((eq? f sin) intsin)
        ((eq? f cube) intcube)
        ((eq? f exp) exp)
        ((eq? f tesseract) inttesseract)))

(define (box func a b)
  (- (func b) (func a)))

(define (exactintegral f a b) (box (integral f) a b))

;(define ( sum-integers a b )
;  (if (> a b)
;      0
;      (+ a (sum-integers (+ a 1) b ))))
;
;(define (sum-cubes a b)
;  (if (> a b)
;      0
;      (+ (cube a) (sum-cubes (+ a 1 ) b ))))
;
;(define (pi-sum a b)
;  (if (> a b)
;      0
;      (+ (/ 1.0 (* a (+ a 2))) ( pi-sum (+ a 4) b))))
;

;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))
;(define (product term a next b)
;  (if (> a b)
;      1
;      (* (term a)
;         (product term (next a) next b))))
;
;(define (sum term a next b)
;  (define (iter a total)
;    (if (> a b) total
;        (iter (next a) (+ total (term a)))))
;  (iter a 0) )

;(define (accumulator identity combiner term a next b)
;  (if (> a b)
;      identity
;      (combiner (term a)
;                (accumulator identity combiner term (next a) next b))))


(define (filtered-accumulate identity combiner term a next b filter)
  (define (iter a total)
    (if (> a b) total
        (iter (next a) (if (filter a) 
                           (combiner total (term a))
                           total))))
  (iter a identity))

(define (accumulator identity combiner term a next b)
  (filtered-accumulate identity combiner term a next b (lambda (x) #t)))

(define (sum term a next b)
  (accumulator 0 + term a next b))

(define (product term a next b)
  (accumulator 1 * term a next b))

(define (sumoverprimes term a b)
  (filtered-accumulate 0 + term a inc b prime?))

(define (inc n) (+ n 1))
(define (identity n) n)
(define (sum-cubes a b) (sum cube a inc b))
(define (sum-integers a b) (sum identity a inc b))
(define (sum-primes a b) (sumoverprimes identity a b))
(define (sum-primes-squared a b) (sumoverprimes square a b))

(define (product-integers a b) (product identity a inc b))

(define (productallrelativelyprimeupto n)
  (filtered-accumulate 1 * identity 1 inc ( - n 1) (lambda (i)(= (GCD i n) 1))))

"sanity check"
(+ 1 2 3 4 5 6 7 8 9 10)
(sum-integers 1 10)
(* 1 2 3 4 5 6 7 8 9 10)
(product-integers 1 10)
(+ 2 3 5 7 )
(sum-primes 1 10)
(+ (* 2 2) (* 3 3) (* 5 5) (* 7 7))
(sum-primes-squared 1 10)
(* 1 3 7 9 )
(productallrelativelyprimeupto 10)

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(define (pi-product a b)
  (define (pi-term n)
    (if (odd? n) (/(+ n 1)(+ n 2)) (/(+ n 2)(+ n 1))))
  (product pi-term a inc b))

(define (pisumapprox n) (* 8 (pi-sum 1 n)))
(define (piproductapprox n) (* 4.0 (pi-product 1 n)))
(define pi20 3.14159265358979323846)

(define (incby dx) (lambda (x) (+ x dx)))

(define (rectangle f a b dx)
  (* dx (sum f a (incby dx) (- b dx))))

(define (midpoint f a b dx)
  (* dx (sum f (+ a (/ dx 2)) (incby dx) b)))

(define (trapezium f a b dx)
  (* (/ dx 2) (+ 
               (sum f a (incby dx) b )
               (sum f (+ a dx) (incby dx) (- b dx))))) 

(define (simpson f a b dx)
  (*  (/ dx 3) (+
                (sum f a (incby dx) b )
                (sum f (+ a dx) (incby dx) (- b dx))
                (* 2 (sum f (+ a dx) (incby (* 2 dx)) ( - b dx))))))

(define (approxintegral func a b rule n)
  (rule func a b (/ 1 n)))

(define (convergerate err n) (/ (err n) (err (* n 2))))
(define (err n)    (- (exact) (approx n)))
(define (conv n)      (convergerate err n))

(define testfunc tesseract)
(define a 0)
(define b 2)
(define approxintegrationrule simpson)

(define (exact) (exactintegral testfunc a b))
(define (approx n) (approxintegral testfunc a b approxintegrationrule n))

;(define (exact) pi20)
;(define (approx n) (piproductapprox n))

;"----"
(define (convergencedisplay n)
  (begin
    (display n) (display "\t")
    (display (approx n)) (display "\t")
    (display (err n)) (display "\t")
    (display (conv n)) (newline)))


;"--exact answer--"
;(exact)

;(define (f n) (productallrelativelyprimeupto n))
;
;(f 1)
;(f 2)
;(f 4)
;(f 8)
;(f 16)
;(f 32)
;(f 64)
;(f 128)
;(f 256)
;(f 512)
;(f 1024)






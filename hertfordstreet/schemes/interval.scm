(define (+interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (*interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (inv-interval y)
   (if (<= (* (upper-bound y)(lower-bound y)) 0) (error "division by zero-spanning interval")
 (make-interval (/ 1 (upper-bound y))
                 (/ 1 (lower-bound y)))))

(define (/interval x y)
  (*interval x (inv-interval y)))

(define (-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (width x)
  (/(- (upper-bound x) (lower-bound x)) 2))

(define (width- a b)
  (width (-interval a b)))

(define (width+ a b)
  (width (+interval a b)))

(define (width* a b)
  (width (*interval a b)))

(define (width/ a b)
  (width (/interval a b)))

(define (make-interval a b) (cons a b))
(define (lower-bound x)(min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

(define (make-centre-width c w)
  (make-interval (- c w) (+ c w)))
(define (centre i)
  (/(+ (lower-bound i) (upper-bound i)) 2))

(define (make-centre-percent c p)
  (make-centre-width c (* c (/ p 100))))

(define a (make-interval 1 2))
(define b (make-interval 3 4))
(define c (make-interval 4 5))
(define d (make-interval -2 -1))
(define e (make-interval -1 1))
(define f (make-interval (/ 1 10000) 1))


;(define a+b (add-interval a b))
;(define a+c (add-interval a c))
;(define a*b (mul-interval a b))
;(define 1/a (invert-interval a))
;(define 1/b (invert-interval b))
;(define a/b (div-interval a b))
;(define a-b (sub-interval a b))

(define r1 (make-interval 100.0 101))
(define r2 (make-interval 200 201))

(/interval(*interval r1 r2)(+interval r1 r2))
(inv-interval (+interval (inv-interval r1)(inv-interval r2)))

(/ 200. 3)
(/ 20301. 302)
(/ 20000. 302)
(/ 20301. 300)





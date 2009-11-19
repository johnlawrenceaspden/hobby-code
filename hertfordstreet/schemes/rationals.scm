(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))


(define (rat a b)
  (if (= b 0) (error "/0 illegal"))
  (if (> a 0)
      (if (> b 0)
          ( let ((n (GCD b a)))
             (cons (/ a n) (/ b n)))
          ( let ((n (GCD (- b) a)))
             (cons (/ (- a) n) (/ (- b) n))))
      (if (> b 0)
          ( let ((n (GCD b (- a))))
             (cons (/ a n) (/ b n)))
          ( let ((n (GCD (- b) (- a))))
             (cons (/ (- a) n) (/ (- b) n))))))


(define nu car)
(define de cdr)

(define (drat r)
  (display (nu r))
  (display "/")
  (display (de r))
  (newline))

(define (+rat a b)
  (rat (+ (*(nu a)(de b)) (*(nu b)(de a)) )
       (*(de a)(de b)) ))

(define (-rat a b)
  (rat (- (*(nu a)(de b)) (*(nu b)(de a)) )
       (*(de a)(de b)) ))

(define (*rat a b)
  (rat (*(nu a)(nu b))
       (*(de a)(de b))))

(define (/rat a b)
  (rat (*(nu a)(de b))
       (*(de a)(nu b))))


(define half  (rat 1 2))
(define third (rat -1 -3))
(define -third (rat -1 3))
(define -half (rat 1 -2))
(define zero (rat 0 10))


(drat half)
(drat third)
(drat -third)
(drat -half)
(drat zero)
(drat (+rat half third))
(drat (-rat -half third))
(drat (*rat half -third))
(drat (/rat -half -third))

(drat (+rat third -third))

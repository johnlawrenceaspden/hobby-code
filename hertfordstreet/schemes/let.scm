(define (f x y)
  (+
   (* x (*(+ 1 (* x y))(+ 1 (* x y))) )
   (* y (- 1 y))
   (*(+ 1 (* x y))(- 1 y))))

(define (f x y)
  (define (1+xy x y) (+ 1 (* x y)))
  (+
   (* x (* (1+xy x y) (1+xy x y)) )
   (* y (- 1 y))
   (*(1+xy x y)(- 1 y))))

(define (f x y)
  (define (1+xy) (+ 1 (* x y)))
  (+
   (* x (* (1+xy) (1+xy)) )
   (* y (- 1 y))
   (*(1+xy)(- 1 y))))

(define (f x y)
  (define (f-helper a b)
    (+(* x a a)
      (* y b)
      (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f x y)
  ((lambda (a b)
    (+(* x a a)
      (* y b)
      (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x a a) (* y b) (* a b))))


"3956420011"
(f 10 2000)





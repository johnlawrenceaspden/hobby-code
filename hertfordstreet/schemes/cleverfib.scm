(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (cleverfib n)
  (T n 0 1 1 0))

(define (T n p q a b)
  (cond ((= n 0) b)
        ((even? n) (T (/ n 2) (+ (* q q) (* p p)) (+ (* q q) (* 2 p q)) a b))
        ((odd? n)  (T (- n 1) p q (+ (* p a) (* q a) (* q b)) (+ (* q a) (* p b))))))

(cleverfib 5)

;0 1 1 2 3 5 8 13 21 34 55


(define (** a b)
  (if (= b 0) 1 (* a (** a (- b 1)))))

(define (cons a b)
  (*(** 2 a)(** 3 b)))

(define (divides? n d) (= (remainder n d) 0))

(define (power n d)
  (if (divides? n d) (+ 1 (power (/ n d) d)) 0))


(define (car a) (power a 2))
(define (cdr a) (power a 3))

(define a (random 10))
(define b (random 10))

a
b
(car (cons a b))
(cdr (cons a b))

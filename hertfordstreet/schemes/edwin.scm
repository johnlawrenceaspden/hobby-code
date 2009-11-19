(define p1
  (lambda (x y)
    (+ (p2 x y)
       (p3 x y))))

(define p2
  (lambda (z w)
    (* z w)))

(define p3
  (lambda (a b)
    (+ (p2 a)
       (p2 b))))

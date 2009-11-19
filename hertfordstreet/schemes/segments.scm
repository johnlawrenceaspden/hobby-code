(define (square x) (* x x))

(define (make-point x y)
  (cons x y))

(define (xpoint p)
  (car p))

(define (ypoint p)
  (cdr p))

(define (average p q)
  (make-point (/(+ (xpoint p) (xpoint q))2)
              (/(+ (ypoint p) (ypoint q))2)))

(define (distance p q)
  (sqrt (+ (square (- (xpoint p) (xpoint q)))
           (square (-(ypoint p) (ypoint q))))))


(define (make-segment a b)
  (cons a b))

(define (start l)
  (car l))

(define (end l)
  (cdr l))

(define (midpoint l)
  (average (start l) (end l)))

(define (length l)
  (distance (start l) (end l))) 

(define a (make-point 3 0))
(define b (make-point 3 4))
(define c (make-point 0 4))
(define d (make-point 0 0))
;
(define hypotenuse (make-segment a c))
;
(midpoint hypotenuse)


(define (make-quadrilateral a b c d)
  (list a b c d))

(define (point q n)
  (cond ((= n 0) (car q))
        ((= n 1) (car (cdr q)))
        ((= n 2) (car (cdr (cdr q))))
        ((= n 3) (car (cdr (cdr (cdr q)))))))

(define (side q n)
  (cond ((= n 0) (make-segment (point q 0)(point q 1)))
        ((= n 1) (make-segment (point q 1)(point q 2)))
        ((= n 2) (make-segment (point q 2)(point q 3)))
        ((= n 3) (make-segment (point q 3)(point q 0))) ))


(define (perimeter box)
  (+ (length (side box 0))
     (length (side box 1))
     (length (side box 2))
     (length (side box 3)))) 

(define (area box)
  (* (length (side box 0))
     (length (side box 1))))

(define box (make-quadrilateral a b c d))

(perimeter box)
(area box)





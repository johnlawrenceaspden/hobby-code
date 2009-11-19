(define (make-mobile left right)  (cons left right))
(define (left-branch m) (car m))
(define (right-branch m) (cdr m))

(define (make-branch length structure) (cons length structure))
(define (branch-length b) (car b))
(define (structure b) (cdr b))

(define (atom? x) (not (pair? x)))
(define (is-weight? b) (atom? (structure b)))
(define (weight b) (structure b))
(define (get-mobile b) (structure b))

(define (total-weight-branch b)
  (if (is-weight? b) (weight b) (total-weight-mobile (get-mobile b))))

(define (total-weight-mobile m)
  (+ (total-weight-branch (left-branch m))
     (total-weight-branch (right-branch m))))

(define (torque b)
  (if (is-weight? b) (*(branch-length b)(weight b))
      (* (branch-length b) (total-weight-mobile (get-mobile b)))))

(define (balanced m)
  (let ((l(left-branch m))
        (r(right-branch m)))
    (and (= (torque l)(torque r))
         (or (is-weight? l)(balanced (get-mobile l)))
         (or (is-weight? r)(balanced (get-mobile r))))))


(define a1 (make-branch 1 4))
(define a2 (make-branch 2 2))
(define a (make-mobile a1 a2))
(define b1 (make-branch 5 a))
(define b2 (make-branch 2 15))
(define b (make-mobile b1 b2))


(define balanced30 
  (make-mobile 
   (make-branch 2 (make-mobile 
                   (make-branch 1 10)
                   (make-branch 2 5)))
   (make-branch 2 15)))

(define unbalanced30 
  (make-mobile 
   (make-branch 2 (make-mobile 
                   (make-branch 1 11)
                   (make-branch 2 4)))
   (make-branch 2 15)))

(define balanced0 
  (make-mobile 
   (make-branch 2 (make-mobile 
                   (make-branch 1 0)
                   (make-branch 2 0)))
   (make-branch 2 0)))

(define (test-mobile m)
  (display m) (display " ")
  (display (total-weight-mobile m)) 
  (display (balanced m)) (newline)
  )

(test-mobile balanced0) ;0 t
(test-mobile balanced30) ; 30 t
(test-mobile unbalanced30) ;30 f
(test-mobile b) ;21 t
(test-mobile a) ;6 t


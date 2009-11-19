(define (square x) (* x x)) 

(define (insphere? x y)
  (< (+(square (- x 1)) (square (- y 2))) (* 3 3) ))

(define (randompointinthingy? lowx lowy bigx bigy inshape?) 
  (let ((x (+ lowx (* (random) (- bigx lowx))))
        (y (+ lowy (* (random) (- bigy lowy)))))
    (insphere? x y)))

(define (monte-carlo-quadrature-iter lowx lowy bigx bigy inshape? trials passed failed)
  (if (= trials 0)
      (/ passed (+ passed failed))
      (if (randompointinthingy? lowx lowy bigx bigy inshape?)
          (monte-carlo-quadrature-iter lowx lowy bigx bigy inshape? (- trials 1) (+ 1 passed) failed)
          (monte-carlo-quadrature-iter lowx lowy bigx bigy inshape? (- trials 1) passed (+ 1 failed))
          )))

(define (monte-carlo-quadrature-fraction lowx lowy bigx bigy inshape? trials)
  (monte-carlo-quadrature-iter lowx lowy bigx bigy inshape? trials 0 0))

(define (monte-carlo-quadrature-area lowx lowy bigx bigy inshape? trials fracfunc)
  (* (* (- bigx lowx) (- bigy lowy)) (fracfunc lowx lowy bigx bigy inshape? trials)))


(define (mcqf lowx lowy bigx bigy inshape? trials)
  (let ((passed 0)
        (failed 0))
    (do ((i 0 (+ i 1))) ((= i trials))
      (if (randompointinthingy? lowx lowy bigx bigy inshape?)
          (set! passed (+ passed 1))
          (set! failed (+ failed 1))))
    (/ passed (+ passed failed))))
      

(define (funcarea trials) (monte-carlo-quadrature-area -5 -5 5 5  insphere? trials monte-carlo-quadrature-fraction))
(define (imperarea trials) (monte-carlo-quadrature-area -5 -5 5 5  insphere? trials mcqf))


(define (funcerr trials) (- (funcarea trials) (* 3 3 pi)))
(define (impererr trials) (- (imperarea trials) (* 3 3 pi)))

   
   
   
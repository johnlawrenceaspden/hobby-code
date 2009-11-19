(define (search f neg pos tolerance)
  (define (close-enough a b) (< (abs (- a b)) tolerance))
  (define (average a b) (/ (+ a b) 2))
  (if (close-enough neg pos) 
      neg
      (let ((mid (average neg pos)))
        (if (> 0 (f mid)) (search f mid pos tolerance) (search f neg mid tolerance)))))

(define (half-interval-method f a b tolerance)
  (let ((aval (f a))
        (bval (f b)))
    (cond ((and (negative? aval) (positive? bval)) (search f a b tolerance))
          ((and (positive? aval) (negative? bval)) (search f b a tolerance))
          (else (error "doom")))))

(define (printsolve fn low high tolerance)
  (let ((x (half-interval-method fn low high tolerance)))
    (printf "x=~s f(x)=~s~%" x (fn x))))

;(printsolve sin 2.0 4.0 0.01)
;(printsolve (lambda (x) (- (* x x x) (* 2 x) 3))  1.0 2.0 0.001)

(define (fixedpoint fn guess tolerance debug)
  (define (close-enough a b) (< (abs (- a b)) tolerance))
  (define (iter old new)
    (if debug (printf "new guess: ~s ~%" new) 0)
    (if (close-enough old new) new (iter new (fn new))))
  (iter guess (fn guess)))

(define (fixedsolved fn guess debug)
  (fixedpoint fn guess 0.00001 debug))

(define (fixedsolve fn guess) (fixedsolved fn guess #f))
(define (fixedsolvedebug fn guess) (fixedsolved fn guess #t))

(define (average a b) (/(+ a b) 2))
(define (averagedamp f) (lambda (x) (average (f x) x)))
(define (naveragedamp n f) (lambda (x) (/(+(f x) (* n x)) (+ n 1))))


;(fixedsolve (lambda (x) (+ x (sin x))) 2.0)
;(fixedsolve cos 1.0)
;(fixedsolve (lambda (y) (+ (sin y) (cos y))) 1.0)
;(fixedsolve (lambda (y) (average (/ 2 y) y)) 1.0)
;(fixedsolve (lambda (y) (+ 1 (/ 1 y))) 0.1)
;(fixedsolvedebug (lambda (x) (/(log 1000) (log x))) 5)
(define (solvex^x y averagedamper)  
  (fixedsolvedebug (naveragedamp averagedamper (lambda (x) (/(log y) (log x)))) 5))

(define (power x a) (exp (* a (log x))))
;(let ((a (solvex^x 1000000 0.4))) (power a a))

(define (sqrt x) (fixedsolve (averagedamp (lambda (y) (/ x y))) 1.0))
(define (cube-root x) (fixedsolve (naveragedamp 2 (lambda (y) (/ x (* y y)))) 1.0)) 

;(sqrt 4)
;(sqrt 5)
;(cube-root 8)
;(cube-root 9)


(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))
(define (square x) (* x x))
;((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/(g x)
          ((deriv g) x)))))

(define (newtons-method g guess)
  (fixedsolvedebug (newton-transform g) guess))

(define (sqrt x) (newtons-method (lambda (s) (-(* s s) x)) 1.0))
(define (cube-root x) (newtons-method (lambda (s) (-(* s s s) x)) 1.0))

;(square (sqrt 2))
;(cube (cube-root 2))

(define (fixed-point-of-transform g transform guess)
  (fixedsolvedebug (transform g) guess))

(define (sqrt x) 
  (fixed-point-of-transform (lambda (y) (/ x y))
                            averagedamp
                            1.0))

(define (sqrt x) 
  (fixed-point-of-transform (lambda (y) (-(square y) x))
                            newton-transform
                            1.0))
;(square (sqrt 2))

(define (cubic a b c) (lambda (x) (+
                                   (* x x x )
                                   (* a x x )
                                   (* b x )
                                   c)))

;((cubic 2 3 5) -2)
;((cubic 2 3 5) -1)
;((cubic 2 3 5) 0)
;((cubic 2 3 5) 1)
;
;((cubic 2 3 5) (newtons-method (cubic 2 3 5) 1))

(define (inc x) (+ x 1))
(define (double f)
  (lambda (x) (f (f x))))

((double inc) 2)
(((double (double double)) inc) 5)














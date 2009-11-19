(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (* x x) (* y y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r theta)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos theta)))
          ((eq? op 'imag-part) (* r (sin theta)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) theta)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)


(define (apply-generic op arg) (arg op))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z)     (apply-generic 'angle z))



(define z (make-from-real-imag 2 2))

(real-part z)
(imag-part z)
(magnitude z)
(angle z)

(define pi 3.14159265358979323846)

(define w (make-from-mag-ang 1 (/ pi 2)))

(real-part w)
(imag-part w)
(magnitude w)
(angle w)


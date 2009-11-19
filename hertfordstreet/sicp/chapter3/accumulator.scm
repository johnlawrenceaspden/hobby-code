(define (make-accumulator x)
  (lambda (amount)
    (begin
      (set! x (+ x amount))
      x)))

(define A (make-accumulator 10))
(A 5)
(A 20)


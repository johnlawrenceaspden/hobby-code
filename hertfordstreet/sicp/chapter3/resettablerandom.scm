(define (rand-update x)
   (modulo (+ (* x 10792) 9) 73))


(define rand
  (let ((seed 1))
  (lambda (x)
  (if (equal? x 'generate)
      (begin
        (set! seed (rand-update seed))
        seed)
      (begin
        (set! seed 1)
        seed)))))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'reset)
(rand 'generate)
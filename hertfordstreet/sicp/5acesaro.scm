(require (lib "trace.ss"))

(define (++ n) (+ n 1))
(define (-- n) (- n 1))

(define-macro (dp x) 
  `(begin 
     (let ((never-use-this-name ,x)) 
       (display (quote ,x))
       (display '=) 
       (display never-use-this-name) 
       (display "\n ") 
       never-use-this-name)))

(define (cesaro n)
  (= 1 (gcd (random n) (random n))))

(define (monte-carlo func? n)
  (define (monty trials passed)
    (if (= 0 trials) passed
        (if (func?)
            (monty (-- trials) (++ passed) )
            (monty (-- trials) passed))))
  (monty n 0))

(define (estimate-pi n)
  (sqrt ( / 6
            (/(monte-carlo (lambda () (cesaro n)) n) n))))


(estimate-pi 100)


;(define (loop i j)
;  (dp (estimate-pi (* i 10)))
;  (when (< i j) (loop (++ i) j)))
;
;(loop 1 100)

(let loop ((i 0))
  (when (< i 10) 
      (display i) 
      (loop (+ i 1))))

(do ((i 100 (+ 10 i))) 
  ((> i 200)) 
  (dp (estimate-pi  i)))

(for (i 100 200 10) prog)









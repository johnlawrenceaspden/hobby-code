(define (cleverfib n)
  (T n 0 1 1 0))

(define (T n p q a b)
  (cond ((= n 0) b)
        ((even? n) (T (/ n 2) (+ (* q q) (* p p)) (+ (* q q) (* 2 p q)) a b))
        ((odd? n)  (T (- n 1) p q (+ (* p a) (* q a) (* q b)) (+ (* q a) (* p b))))))


(define (fibonacci-tree n) 
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else ( + (fibonacci-tree (- n 1))
                  (fibonacci-tree (- n 2)) ))))

(define (fibiter a b count n)
  (if (= count n) 
      a
      (fibiter b (+ a b) (+ 1 count) n)))

(define (fibonacci-iteration n)
  (fibiter 0 1 0 n))

(define (fibonacci-approximation n)
  (define psi (/ (+ 1 (sqrt 5)) 2))
  (define (power a n)
    (if (= n 0) 1 (* a (power a (- n 1)))))
  (/(power psi n)(sqrt 5)))


(define (fiblist n fibfunc)
  (define (printfib n)
    (display "fib(")
    (display n)
    (display "):")
    (display (fibfunc n)))
  (define (fiblistiter count)
    (printfib count)
    (newline)
    (if (< count n)
        (fiblistiter (+ count 1))
        (begin (display "-------") (newline))))
  (fiblistiter 0)
  )

(define (fibcomp n)
(fiblist n fibonacci-iteration)
;(fiblist n fibonacci-tree)
;(fiblist n fibonacci-approximation)
(fiblist n cleverfib))






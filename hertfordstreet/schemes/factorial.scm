(define (l) (load "factorial"))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (define (factiter total n)
    (if (= n 1)
	total
	(factiter (* total n) (-1+ n))))
  (factiter 1 n))

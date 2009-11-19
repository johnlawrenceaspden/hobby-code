(define (pascal n a)
  (cond ((= n a) 1)
        ((= 0 a) 1)
        (else (+(pascal (- n 1) a) (pascal (- n 1) (- a 1))))))

(define (for a b func)
  (define (foriter n b)
    (func n)
    (if (= n b) 0 (foriter (+ n 1) b)))
  (foriter a b))

(define (pasc n)
  (lambda (a) (begin (display (pascal n a)) (display " "))))

(define (pascline n)
  (for 0 n (pasc n))
  (newline))

(define (pasctriangle n)
  (for 0 n pascline))

(pasctriangle 10)
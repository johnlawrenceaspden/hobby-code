(define (++ n) (+ n 1))
(define (-- n) (- n 1))

(define-macro (dp x) `(begin (display (quote ,x)) (display '=)(display ,x)(display " ")))

;(define (addrecurse n m)
;  (cond ((= n 0) m)
;        (else (++ (addrecurse (-- n) m)))))
;
;(define (additer n m)
;  (cond ((= n 0) m)
;        (else (additer (-- n) (++ m)))))
;
;(define count 1)
;
;(define (demo x)
;  (set! count (++ count))
;  (+ x count))
;
;(define (factorial n)
;  (define (iter m i)
;    (if (> i n) m (iter (* m i) (+ i 1))))
;  (iter 1 1))
;
;(define (factorial! n)
;  (define m 1)
;  (define (loop)
;    (if (= n 1) m 
;        (begin
;          (set! m (* m n))
;          (set! n (- n 1))
;          (loop))))
;  (loop))

(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (+ n 1))
      (dp n)
      n)))

(define c1 (make-counter 0))

(define c2 (make-counter 10))

                    
                 










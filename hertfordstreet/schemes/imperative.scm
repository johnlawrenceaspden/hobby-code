(define (factorial n)
  (if (> n 1) (* n (factorial (- n 1))) 1))


(factorial 10)

(define (factorial n)
  (define (loop total n)
    (if (> n 1) (loop (* total n) (- n 1)) total))
  (loop 1 n))

(factorial 10)

(define (factorial n)
  (let ((m 1)(i 1)) 
    (define (loop)
      (if (<= i n) 
          (begin (set! m (* m i))
                 (set! i (+ i 1))
                 (loop))
          m
          ))(loop)))

(factorial 10)

(define (factorial n)
  (let ((m 1)(i 1)) 
    (define (loop)
      (cond ((<= i n)            
             (set! m (* m i))
             (set! i (+ i 1))
             (loop))
            (else m))
      )(loop)))

(factorial 10)
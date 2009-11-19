; 0 1 2 4 11 25 59

(define ( frec n )
  (if (< n 3) n (+ (frec (- n 1)) (* 2 (frec (- n 2))) (* 3 (frec (- n 3))))))

(define (f n) 
  (define (next a b c) (+ (* 3 a) (* 2 b) c))
  (define (fiter count a b c)
    (if (= n count)
        a
        (fiter (+ 1 count) b c (next a b c)))) 
 (fiter 0 0 1 2) )







(define (list n func name)
  (define (printval n)
    (display name)
    (display "(") 
    (display n)
    (display "):")
    (display (func n)))
  (define (iter count)
    (printval count)
    (newline)
    (if (< count n)
        (iter (+ count 1))
        (begin (display "-------") (newline))))
  (iter 0)
  )

(define (comp n)
  (list n frec "recursive")
  (list n f "iterative"))


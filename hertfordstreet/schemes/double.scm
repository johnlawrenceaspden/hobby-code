(define (double f) (lambda (x) (f (f x)))) ;scheme functions are more general than the functions of mathematics!

(define (inc x) (+ x 1))
(define (square x) (* x x))

(inc 0)
((double inc)0)
(((double double) inc) 0)
((double (double inc)) 0)
((double(double(double inc))) 0)
((((double double) double) inc) 0)


(define (compose f g) (lambda (x) (f (g x))))

((compose square inc) 6)

(define (repeated f n)
  (if (> n 1) (compose (repeated f (- n 1)) f) f))

((repeated square 2) 5)

(define a (repeated inc 10))

(a 10)
(a 20)

(define (smooth f) (lambda (x)
  (define dx 0.1)
  (/ (+ (f (- x dx))
        (f x)
        (f (+ x dx)))
     3)))  

(sin 1)
((smooth sin) 1)
((smooth(smooth sin)) 1)

(define (nfoldsmooth n f)
  ((repeated smooth n) f))

((nfoldsmooth 2 sin) 1)






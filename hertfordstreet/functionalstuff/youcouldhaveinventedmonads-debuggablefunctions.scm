#lang scheme

;first example debuggable functions
(define (square x)
  (list "squaring; " (* x x)))

(define (cube x)
  (list "cubing; " (* x x x)))

;difficult to compose these functions
(let* ((x (square 5))
       (s (car x))
       (y (cadr x)))
  (let* ((q (cube y))
        (t (string-append (car q) s))
        (z (cadr q)))
    (list t z)))

;here's one way
(define (combine square cube x)
  (let* ((a (cube x))
         (text1 (car a))
         (result1 (cadr a)))
    (let* ((b (square result1))
           (text2 (car b))
           (result2 (cadr b)))
      (list (string-append text1 text2) result2))))

;now we can write
(combine square cube 2)
;but I can't see how to do (cube (square (cube 3)))


;bind function takes a debuggable and turns it into (debuggable output)-> (debuggable output)
(define (bind f)
  (lambda (l)
    (let* ((string (car l))
           (value  (cadr l)))
      (let* ((fval (f value))
             (string2 (car fval))
             (result2 (cadr fval)))
        (list (string-append string string2) result2)))))

;and this allows relatively trouble free composition
((bind cube) (square 4))
((bind square) ((bind cube) (square 4)))
((bind cube) ((bind cube) ((bind cube) (square 3))))

;what does the identity debuggable function look like?
(define (unit x)
  (list "" x))

(square 5)
((bind unit) (square 5))
((bind square) (unit 5))

;we can lift any function into a debuggable one
(define (lift f)
  (lambda (x) (list "" (f x))))

((lift sin) 2)

((bind square) ((lift sin) 1))

(sin (cos 2))

;lifting a composition
((lift (lambda (x) (sin (cos x)))) 2)
;is the same as bind-composing two lifts
((bind (lift sin)) ((lift cos) 2))

;sin of cos of square of cube of sin of x can now be expressed:
((bind (lift sin)) 
 ((bind (lift cos)) 
  ((bind square) 
   ((bind cube) 
    ((lift sin) 2)))))

;which should be a debugging version of 
(sin 
 (cos 
  ((lambda (x) (* x x)) 
   ((lambda (x) (* x x x))
    (sin 2)))))



;a modest program to calculate exponents exp x = 1+x+x^2/2!+x^3/3!+.....

(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

(define (factorial n)
  (if (= n 0) 
      1
      (* n (factorial (- n 1)))))

(define (exponential-by-powerseries x n)
  (if (= n 0)
      1
      (+ (/ (power x n) (factorial n)) (exponential-by-powerseries x (- n 1)))))

;an example of its use:

(display "exp(2) to twenty terms: ") 
(exponential-by-powerseries 2 20)

;deep wizardry

(define builtin-* *)
(define builtin-+ +) 
(define builtin-/ /)

(define (emit-* x y)
  (cond ((or (eqv? x 0) (eqv? y 0)) 0)
        ((eqv? x 1) y)
        ((eqv? y 1) x)
        (else (emit-binary-op builtin-* '* x y))))

(define (emit-+ x y)
  (cond ((eqv? x 0) y)
        ((eqv? y 0) x)
        (else (emit-binary-op builtin-+ '+ x y))))

(define (emit-/ x y)
  (cond ((eqv? y 1) x)
        (else (emit-binary-op builtin-/ '/ x y))))

(define (emit-binary-op op op-name x y)
  (cond ((and (number? x) (number? y))
         (op x y))
        ((and (equal? x y)
              (not (symbol? x)))
         `(let ((y ,x))
            (,op-name y y)))
        (else
         `(,op-name ,x ,y))))

(define (partially-evaluate x)
  (fluid-let ((+ emit-+)(* emit-*)(/ emit-/))
    (eval x)))

;example of wizardry

(define first-five-terms (partially-evaluate '(exponential-by-powerseries 'x 5)))

(display "the powerseries for the first five terms is:\n")
(display first-five-terms) (newline)

;now we can use this as a specialized function

(define lambda-list (list 'lambda '(x) first-five-terms))

(define (exp-five x)
  (apply (eval lambda-list) (list x)))

(exp-five 2)
(exp-five 3)

;timings

(define testlist '(1 2 3 4 5 6 7 8 9 10))

(time (map (lambda (x) (exponential-by-powerseries x 5)) testlist))
(time (map exp-five testlist))
;see http://cybertiggyr.com/gene/peval/peval.cgi for the inspiration for this lunacy

;a partial evaluator automatically specializes a program with respect to some of its input.

(define (check-macro input)
  (syntax-object->datum (expand-once input)))

;function to evaluate polynomials

(define (poly-value coeffs x)
  (foldl (lambda (coeff value)
           (+ (* value x) coeff))
         0
         coeffs))

;example
(display "5x^2+1: x=2\n")
(poly-value '( 5 0 1) 2)

;functions to efficiently calculate powers

(define (square x) (* x x))

(define (power x n)
  (cond ((= n 0) 1)
        ((odd? n) (* x (power x (- n 1))))
        (else (square (power x ( / n 2))))))

;example
;(require (lib "trace.ss"))
;(trace + * square power)
(display "2^31\n")
(power 2 31)
;(untrace + * square power)
 

;witchcraft
(display "partial evaluation of polynomial\n")
(define (emit-+ a b)
  `(+ ,a ,b))

(define (emit-* a b)
  `(* ,a ,b))

(fluid-let ((* emit-*)(+ emit-+))
  (poly-value '(5 0 1) 'x))

;better witchcraft
(display "again with cleverer operators\n")
(define built-in-* *)
(define (emit-sexy-* a b)
  (cond ((equal? a 0) 0)
        ((equal? b 0) 0)
        ((equal? a 1) b)
        ((equal? b 1) a)
        ((and (number? a) (number? b) (built-in-* a b)))
        (else `(* ,a ,b))))

(define built-in-+ +)
(define (emit-sexy-+ a b)
  (cond ((equal? a 0) b)
        ((equal? b 0) a)
        ((and (number? a) (number? b) (built-in-+ a b)))    
        (else `(+ ,a ,b))))

(fluid-let ((* emit-sexy-*)(+ emit-sexy-+))
  (poly-value '(5 0 1) 'x))

;wrap the witchcraft up
(define (compile exp)
  (fluid-let ((* emit-sexy-*)(+ emit-sexy-+))
    (eval exp)))

;examples
(display "some more partially evaluated polynomials\n")
(compile '(poly-value '(5 0 1) 2))
(compile '(poly-value '(10 0 0 1 5 0 1) 'x))

;nasty redundancy in the square function
(display "but the power function is not so neat x^10 ->\n")
(compile '(power 'x 10))

(display "remove the redundant computations\n")
(define (emit-square x)
  `(let ((y ,x)) (* y y)))

(fluid-let ((* emit-sexy-*)(+ emit-sexy-+)(square emit-square))
  (power 'x 10))

(display "too many lets:\n")

(define (emit-sexy-square x)
  (if (symbol? x)
      `(* ,x ,x)
      `(let ((y ,x))
         (* y y))))

(fluid-let ((* emit-sexy-*)(+ emit-sexy-+)(square emit-sexy-square))
  (power 'x 10))

;
;(define (square x)
;  (* x x ))
;
;(define (emit-power x n)
;  (cond ((= n 0) 1)
;        ((= n 1) x)
;        ((odd? n) `(* ,x ,(emit-power x (- n 1))))
;        (else (emit-square (emit-power x (/ n 2))))))
;
;(define (emit-square x)
;  (if (pair? x)
;      `(let ((y ,x)) (* y y))
;      `(* ,x ,x)
;      ))
;
;(define-macro (pp a b)
;  (eval (list 'emit-power a b)))
;
;(check-macro '(pp 22 6))
;(pp 22 6)
;(* 22 22 22 22 22 22)





































;(define (square x) (* x x))
;
;(define (power x n)
;  (cond ((= n 0) 1)
;        ((odd? n) (* x (power x (- n 1))))
;        (else (square (power x ( / n 2))))))
;
;(power 10 7)
;
;(define (emit-power x n)
;  (cond ((= n 0) 1)
;        ((odd? n) `(* x ,(emit-power x (- n 1))))
;        (else `(square ,(emit-power x (/ n 2))))))
;
;(emit-power 'x 7)
;
;(define (emit-power x n)
;  (cond ((= n 0) 1)
;        ((= n 1) 'x)
;        ((odd? n) `(* x ,(emit-power x (- n 1))))
;        (else `(square ,(emit-power x (/ n 2))))))
;
;(emit-power 'x 7)
;
;(define (emit-power x n)
;  (cond ((= n 0) 1)
;        ((= n 1) 'x)
;        ((odd? n) `(* x ,(emit-power x (- n 1))))
;        (else (emit-square (emit-power x (/ n 2))))))
;
;(define (emit-square x)
;  `(* ,x ,x))
;
;(emit-power 'x 7)
;
;(define (emit-square x)
;  `(let ((y ,x))
;     (* y y)))
;
;(emit-power 'x 7)
;
;(define (emit-square x)
;  (if (symbol? x)
;      `(* ,x ,x)
;      `(let ((y ,x))
;         (* y y))))
;
;(emit-power 'x 7)
;(emit-power 'input 2) ;sigh
;
;(define (emit-power x n)
;  (cond ((= n 0) 1)
;        ((= n 1) x)
;        ((odd? n) `(* ,x ,(emit-power x (- n 1))))
;        (else (emit-square (emit-power x (/ n 2))))))
;
;(emit-power 'x 7)
;(emit-power 'input 2)
;

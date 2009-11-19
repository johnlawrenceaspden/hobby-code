#lang scheme

;from the macro stepper paper
;scheme2006.cs.uchicago.edu/10-culpepper.pdf

(define-syntax def-false
  (syntax-rules ()
    ((def-false a ...) ((define a #f) ...))))

;(def-false x y z)

(define-syntax (or stx)
  (syntax-case stx ()
    [(or e1 e2)
     (syntax (let ((tmp e1)) (if tmp tmp e2)))]
    [(or e1 e2 ...)
     (syntax (let ((tmp e1)) (if tmp tmp (or e2 ...))))]))

(or #t #f )

(or #t #f #f)


(define-syntax (if-it1 stx) ;wrong! it is caught by hygiene mechanism
  (syntax-case stx ()
    [(if-it1 test then else)
     (syntax
      (let ((it test))(if it then else)))]))

;(if-it1 (assoc 2 '((2 . 'a))) (print it) (print "no"))

(define-syntax (if-it2 stx)
  (syntax-case stx ()
    [(if-it2 test then else)
     (with-syntax
         ([it (datum->syntax #'if-it2 'it)])
       (syntax
        (let ([it test])
          (if it then else))))]))

(if-it2 (assoc 2 '((2 . a))) (print (cdr it)) (print "no"))






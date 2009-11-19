(require (lib "1.ss" "srfi"))

(define (atom? a) (not (pair? a)))

(define (value? exp) (and (null? (cdr exp)) (atom? (car exp))))
(define (value exp) (car exp))

(define (listofonelist? l) (and (null? (cdr l)) (list? (car l))))

(define (bracketedthing? exp) (listofonelist? exp))
(define (removebrackets exp) (car exp))

(define ((operatorpredicate op) x) (equal? x op))
(define ((not-operatorpredicate op) x) (not ((operatorpredicate op) x)))
(define ((operatorexpression op) exp) (any (operatorpredicate op) exp))
(define ((rafirstarg op) exp) (take-while (not-operatorpredicate op) exp))
(define ((rasecondarg op) exp) (cdr (drop-while (not-operatorpredicate op) exp) ))
(define ((lafirstarg op) exp) (reverse ((rasecondarg op) (reverse exp))))
(define ((lasecondarg op) exp) (reverse ((rafirstarg op) (reverse exp))))

(define ((makelispversion op) a b) (list op a b))

(define (listize x) (if (list? x) x (list x)))
(define ((makeforthversion op) a b) (append (listize a) (listize b) (list op)))


(define ((ratransform generator op) exp)
  ((generator op)
   ((evaluate generator) ((rafirstarg op) exp))
   ((evaluate generator) ((rasecondarg op) exp))))

(define ((latransform generator op) exp)
  ((generator op)
   ((evaluate generator) ((lafirstarg op) exp))
   ((evaluate generator) ((lasecondarg op) exp))))

(define ((evaluate generator) exp)
  ;(display exp) (newline)
  (cond ((value? exp) (value exp))
        (((operatorexpression '+) exp) ((ratransform generator '+) exp))
        (((operatorexpression '-) exp) ((ratransform generator '-) exp))
        (((operatorexpression '*) exp) ((ratransform generator '*) exp))
        (((operatorexpression '/) exp) ((latransform generator '/) exp))
        ((bracketedthing? exp) ((evaluate generator) (removebrackets exp)))
        (else exp)))

(define (infixtoschemetest testthing value)
  (if (equal? value (eval (compiletolisp testthing)))
      (display "pass ")
      (begin 
        (newline)
        (display "fail: ") (display testthing) (newline)
        (display "evaluates to:") (display (compiletolisp testthing)) (newline)
        (display "which is equal to:") (display (eval (compiletolisp testthing)))
        (display " not:")(display value) (newline)
      
        )))

(define compiletolisp (evaluate makelispversion))
(define compiletoforth (evaluate makeforthversion))
      

(define x 25)
(infixtoschemetest '( (2 + 3) * 4 + 5 - x + 50 / (5 + 5 + 3 * 5) - (10 / ((1 + 1) * 2 + 1))) 0)
(infixtoschemetest '( 15.0 / 3 / 5 / 5 ) 0.2 )
(newline)

;(compiletolisp '(a / b / c / d))
;(compiletoforth '(a / b / c / d))
;
;(compiletolisp '((a + b) + c + d))
;(compiletoforth  '((a + b) + c + d))
;(compiletolisp '(a + b + c + d))
;(compiletoforth '(a + b + c + d))





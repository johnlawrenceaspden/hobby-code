#lang scheme
(require (lib "trace.ss"))

;taken from sicp lecture 7b where we add the (lambda (x . y) ...) syntax

;the heart of the evaluator
(define (eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup exp env))
        ((eq? (car exp) 'quote) (cadr exp))
        ((eq? (car exp) 'lambda)
         (list 'closure (cdr exp) env))
        ((eq? (car exp) 'cond)
         (evcond (cdr exp) env))
        (else (my-apply (eval (car exp) env) (evlist (cdr exp) env)))))

(define (my-apply proc args)
  (cond ((primitive? proc) (apply-primop proc args))
        ((eq? (car proc) 'closure)
         (eval (cadadr proc) (bind (caadr proc) args (caddr proc))))
        (else (list 'applyerror proc args))))

(define (evlist explist env)
  (if (null? explist) '()
      (cons (eval (car explist) env) (evlist (cdr explist) env))))              

(define (evcond clauses env) 
  (cond ((null? clauses) 'conderror clauses env)
        ((eq? (caar clauses) 'else)
         (eval (cadar clauses) env))
        ((false? (eval (caar clauses) env))
         (evcond (cdr clauses) env))
        (else (eval (cadar clauses) env))))

;;construction of environments ( I think mine is better than Sussman's )
(define (bind vars vals env) (append (pair-up vars vals) env))

(define (pair-up vars vals)
  (cond
    ((and (eq? vars '())(eq? vals '())) '())
    ((eq? vals '()) (error 'too-few-values))
    ((eq? vars '()) (error 'too-many-values))
    ((pair? vars) (cons (cons (car vars) (car vals)) 
                        (pair-up (cdr vars) (cdr vals))))
    (else (list (cons vars vals )))))

(define (lookup exp env)
  (cond ((null? env) (error (format "lookup error ~a not bound in environment ~a" exp env)))
        ((eq? (car env) exp) (cdr env))
        ((eq? (caar env) exp) (cdar env))
        (else (lookup exp (cdr env)))))

;pre-defined primitive ops
(define (primitive? op) (or (eq? op 'prim-times) 
                            (eq? op 'prim-equals)
                            (eq? op 'prim-car)
                            (eq? op 'prim-cdr)
                            (eq? op 'prim-cons)))
(define (apply-primop op args)
  (cond ((eq? op 'prim-times)  (apply * args))
        ((eq? op 'prim-equals) (apply = args))
        ((eq? op 'prim-car)    (apply car args))
        ((eq? op 'prim-cdr)    (apply cdr args))
        ((eq? op 'prim-cons)   (apply cons args))
        ))

(define init-env '((* . prim-times)(= . prim-equals)(car . prim-car)(cdr . prim-cdr)(cons . prim-cons)))



;tests
(define (tests)
  (if (and 
       (equal? (lookup 'z (pair-up '(x y z) '(1 2 3))) 3)
       (equal? (map (λ(q) (lookup q (bind '(w z) '(20 50) (pair-up '(x y z) '(1 2 3))))) '(x y z w)) '(1 2 50 20))
       (equal? (eval '2 '()) 2)
       (equal? (eval '(quote x) '()) 'x)
       (equal? (eval '(lambda x (* 2 x)) '((* . prim-times))) '(closure (x (* 2 x)) ((* . prim-times))))
       (equal? (eval 'x '((x . 4))) 4)
       (equal? (eval '((lambda (x)(* x x)) 2) '((* . prim-times))) 4)
       (equal? (eval '(cond ((= x 1) 'one)((= x 2) 'two)(else 'other)) (bind '(x) '(2) init-env)) 'two)
       (equal? (eval '(cond ((= x 1) 'one)((= x 2) 'two)(else 'other)) (bind '(x) '(1) init-env)) 'one)
       (equal? (eval '(cond ((= x 1) 'one)((= x 2) 'two)(else 'other)) (bind '(x) '(3) init-env)) 'other)
       (equal? (eval '((lambda (x) (cond ((= x 1) 'one)((= x 2) 'two)(else 'other))) 1) init-env) 'one)
       (equal? (eval '((lambda (x) (cond ((= x 1) 'one)((= x 2) 'two)(else 'other))) 2) init-env) 'two)
       (equal? (eval '((lambda (x) (cond ((= x 1) 'one)((= x 2) 'two)(else 'other))) 3) init-env) 'other)
       (equal? (eval '((lambda(x)(* x x)) 2) init-env) 4)
       (equal? (eval '(((lambda (y) (lambda (x) (* y x))) 2) 3) init-env) 6)      
       (equal? (map (λ(x) (lookup x (bind '() '() (bind '(x z) '(10 2) (bind '(x . y) '(1 2 3 4) init-env))))) '(x z y * =)) '(10 2 (2 3 4) prim-times prim-equals))
       (equal? (eval '((lambda (x . y) (* x (* (car y)(car (cdr y))))) 1 2 3 4) init-env) 6 )
       (equal? (eval '((lambda y (* (car y) (car (cdr y)) (car (cdr (cdr y))))) 1 2 3 4) init-env) 6)
       (equal? (eval '((lambda x x) 1 2 3 4 5) '()) '(1 2 3 4 5))
       (equal? (eval '((lambda 2 (* 2 2)) 3) init-env) 4)
       (equal? (eval '((lambda x (* (car x) (car (cdr x)))) 3 4) init-env) 12)     
       (equal?  (eval '((lambda () (* x (* (car y) (car (cdr y)))))) (bind '(x y) '(10 (2 3)) init-env)) 60)
       (equal? '_ '_)
       (equal? '_ '_)
       (equal? '_ '_))
      "pass" 
      "FAIL!!!!!!!!!!!!!!!"))

(define (failtests)
  (if (for/and ((i (list (with-handlers (((λ(v) #t) (λ(v) (list 'failed v) )))
                           (lookup 'w (pair-up '(x y z) '(1 2 3))))
                         (with-handlers (((λ(v) #t) (λ(v) (list 'failed v) )))
                           (map (λ(q) (lookup q (bind '(w z) '(20 50) (pair-up '(x y z) '(1 2 3))))) '(u))))))
               (eq? (car i) 'failed))
      "pass: failing tests all failed"
      "FAIL!!!!!!!!!!!!!!!!!: failing tests failed to fail"))

(define (alltraceon) (trace eval my-apply bind pair-up lookup evlist evcond))
(define (alltraceoff) (untrace eval my-apply bind pair-up lookup evlist evcond))
(tests)
(failtests)

;now turn all tracing on so that we can watch evaluation at the repl

(alltraceon)

(printf "try: ~a~n" '(eval '((lambda (x . y) (* x (* (car y)(car (cdr y))))) 1 2 3 4) init-env))

(eval '((lambda (x . y) (* x (* (car y) (car (cdr y))))) 1 2 3 4) init-env)
#lang scheme
(require (lib "trace.ss"))

;the heart of the evaluator
;dynamic scope
(define (dynamic-eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup exp env))
        ((eq? (car exp) 'quote) (cadr exp))
        ((or (eq? (car exp) 'λ) (eq? (car exp) 'lambda)) exp)
        ((eq? (car exp) 'cond) (evcond (cdr exp) env))
        (else (dynamic-apply (eval (car exp) env) (evlist (cdr exp) env) env))))

(define (dynamic-apply proc args env)
  (cond ((primitive? proc) (apply-primop proc args))
        ((or (eq? (car proc) 'lambda) (eq?(car proc) 'λ)) (eval (caddr proc) (bind (cadr proc) args env)))
        (else (error (format "dynamic-apply error: trying to apply ~a to ~a in environment ~a" proc args env)))))

;lexical scope
(define (lexical-eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup exp env))
        ((eq? (car exp) 'quote) (cadr exp))
        ((or (eq? (car exp) 'λ) (eq? (car exp) 'lambda))
         (list 'closure (cdr exp) env))
        ((eq? (car exp) 'cond)
         (evcond (cdr exp) env))
        (else (lexical-apply (eval (car exp) env) (evlist (cdr exp) env)))))

(define (lexical-apply proc args)
  (cond ((primitive? proc) (apply-primop proc args))
        ((eq? (car proc) 'closure)
         (eval (cadadr proc) (bind (caadr proc) args (caddr proc))))
        (else (error (format "lexical-apply error: trying to apply ~a to ~a" proc args)))))

;choose which one to use
(define eval lexical-eval)

;evaluator common procedures
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
        ((eq? (caar env) exp) (cdar env))
        (else (lookup exp (cdr env)))))

;pre-defined primitive ops
(define (primitive? op) (or (eq? op 'prim-times) 
                            (eq? op 'prim-equals)
                            (eq? op 'prim-car)
                            (eq? op 'prim-cdr)
                            (eq? op 'prim-cons)
                            (eq? op 'prim-minus)
                            (eq? op 'prim-lessthan)))
(define (apply-primop op args)
  (cond ((eq? op 'prim-times)  (apply * args))
        ((eq? op 'prim-equals) (apply = args))
        ((eq? op 'prim-car)    (apply car args))
        ((eq? op 'prim-cdr)    (apply cdr args))
        ((eq? op 'prim-cons)   (apply cons args))
        ((eq? op 'prim-lessthan)   (apply < args))
        ((eq? op 'prim-minus)  (apply - args))
        ))

(define init-env '((* . prim-times)(= . prim-equals)(car . prim-car)(cdr . prim-cdr)(cons . prim-cons)(- . prim-minus)(< . prim-lessthan)))


;tests
(define-syntax-rule (test expression expected-result)
  (let ((expstring (format "~a" (quote expression))))
    (with-handlers (((λ(v) #t) (λ(v) (list #f (format "~a failed with [~a]" expstring v)))))
      (let* ((result expression)
             (success (equal? result expected-result)))
        (list success (format "~a -> ~a (should be ~a)" expstring result expected-result))))))

(define-syntax-rule (failtest expression)
  (let ((expstring (format "~a" (quote expression))))
    (with-handlers (((λ(v) #t) (λ(v) (list #t (format "~a failed with [~a]" expstring v)))))
      (let ((result expression))
        (list #f (format "~a should have failed, but completed, returning ~a" expstring result))))))

(define (test-testing-tests)
  (list
   (failtest (lookup 'z (pair-up '(x y z) '(1 2 3))))
   (test (lookup 'z (pair-up '(x y z) '(1 2 3))) 4)
   (test (lookup 'w (pair-up '(x y z) '(1 2 3))) 3)
   ))

(let ((ttt (test-testing-tests)))
  (if (for/or ((i ttt)) (car i)) 
      (error (format "test-testing-tests are broken ~a" (map cdr (filter car ttt))))
      "tests tested successfully"))

(define (environmenttests)
  (list
   (test     (lookup 'z (pair-up '(x y z) '(1 2 3))) 3)
   (failtest (lookup 'w (pair-up '(x y z) '(1 2 3))))
   (test     (map (λ (q) (lookup q (bind '(w z) '(20 50) (pair-up '(x y z) '(1 2 3))))) '(x y z w)) '(1 2 50 20))
   (failtest (lookup 'u (bind '(w z) '(20 50) (pair-up '(x y z) '(1 2 3)))))
   (test     (map (λ (x) (lookup x (bind '() '() (bind '(x z) '(10 2) (bind '(x . y) '(1 2 3 4) init-env))))) '(x z y * =))
             '(10 2 (2 3 4) prim-times prim-equals))
   (failtest (bind '(x) '(1 2) init-env))
   (failtest (bind '(x y) '(1) init-env))))

(define (applytests)
  (list
   (failtest (dynamic-apply '(closure 'a 'b 'c) '() '()))
   (failtest (lexical-apply '(lambda 'a 'b 'c) '()))))

(define (evaltests)
  (list  
   (test     (eval '2 '()) 2)
   (test     (eval '(quote x) '()) 'x)
   (test     (eval 'x '((x . 4))) 4)
   (test     (eval '((λ (x) (* x x)) 2) '((* . prim-times))) 4)
   (test     (eval '((lambda (x) (* x x)) 2) '((* . prim-times))) 4)
   (test     (eval '(cond ((= x 1) 'one) ((= x 2) 'two) (else 'other)) (bind '(x) '(2) init-env)) 'two)
   (test     (eval '(cond ((= x 1) 'one) ((= x 2) 'two) (else 'other)) (bind '(x) '(1) init-env)) 'one)
   (test     (eval '(cond ((= x 1) 'one) ((= x 2) 'two) (else 'other)) (bind '(x) '(3) init-env)) 'other)
   (test     (eval '((λ (x) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) 1) init-env) 'one)
   (test     (eval '((λ (x) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) 2) init-env) 'two)
   (test     (eval '((λ (x) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) 3) init-env) 'other)
   (test     (eval '((λ (x) (* x x)) 2) init-env) 4)
   (test     (eval '((λ (x . y) (* x (* (car y) (car (cdr y))))) 1 2 3 4) init-env) 6)
   (test     (eval '((λ y (* (car y) (car (cdr y)) (car (cdr (cdr y))))) 1 2 3 4) init-env) 6)
   (test     (eval '((λ x x) 1 2 3 4 5) '()) '(1 2 3 4 5))
   (test     (eval '((λ 2 (* 2 2)) 3) init-env) 4)
   (test     (eval '((λ x (* (car x) (car (cdr x)))) 3 4) init-env) 12)
   (test     (eval '((λ () (* x (* (car y) (car (cdr y)))))) (bind '(x y) '(10 (2 3)) init-env)) 60)
   (test     (eval '((lambda(x) (cons (cdr x)(car x))) (cons 'a 'b)) init-env) '(b . a))
   ))


(define cuteexp '((λ (f) ((λ (x) (f)) 'dynamic)) ((λ (x) (λ () x)) 'lexical)))

(define (dynamic-binding-tests)
  (list
   (test     (eval '((λ (p y) (p 5)) (λ (x) (* x y)) 10) init-env) 50)
   (test     (eval '((λ (p y) (p 5)) (λ (x) (* x y)) 10) init-env) 50)
   (failtest (eval '(((λ (y) (λ (x) (* y x))) 2) 3) '((* . prim-times))))
   (test (eval '((λ (p y) (p 5)) ((λ (y) (λ (x) (* x y))) 100) 10) '((* . prim-times))) 50)
   (test (eval '((λ (f)   (* ((λ (x) (f)) 3)((λ (x) (f)) 4))) ((λ (x) (λ () x)) 2)) '((* . prim-times))) 12)
   (test (eval cuteexp init-env) 'dynamic)))

(define (lexical-binding-tests)
  (list
   (failtest     (eval '((λ (p y) (p 5)) (λ (x) (* x y)) 10) init-env))
   (failtest     (eval '((λ (p y) (p 5)) (λ (x) (* x y)) 10) init-env))
   (test (eval '(((λ (y) (λ (x) (* y x))) 2) 3) '((* . prim-times))) 6 )
   (test (eval '((λ (p y) (p 5)) ((λ (y) (λ (x) (* x y))) 100) 10) '((* . prim-times))) 500)
   (test (eval '((λ (f)   (* ((λ (x) (f)) 3)((λ (x) (f)) 4))) ((λ (x) (λ () x)) 2)) '((* . prim-times))) 4)
   (test (eval cuteexp init-env) 'lexical)
   ))

(define (dotests . testprocs)
  (let* ((t (append-map (λ(p)(p)) testprocs ))
         (success (for/and ((i t)) (car i)))
         (failingtests (map (λ(s) (cadr s))(filter (λ(s) (not (car s))) t))))
    (if success "all tests pass" (list "!!!!! TEST FAILURE!!!!!->" failingtests))))

(define (lexical-binding)
  (set! eval lexical-eval)
  (printf "evaluating with lexical binding: ~n"))

(define (dynamic-binding)
  (set! eval dynamic-eval)
  (printf "evaluating with dynamic binding: ~n"))


(dynamic-binding)
(dotests environmenttests evaltests applytests dynamic-binding-tests)

(lexical-binding)
(dotests environmenttests evaltests applytests lexical-binding-tests)

(lexical-binding)

;towards an interactive top level
(define (toplevel)
  (let ((entered (read)))
    (if (and (pair? entered) (eq? (car entered) 'define))
        (set! init-env (bind (cadr entered) (eval (caddr entered) init-env) init-env))
        (printf "~a~n" (eval entered init-env)))
    (toplevel)))

(define (read-exp exp)
  (if (and (pair? exp) (eq? (car exp) 'define))
      (set! init-env (bind (cadr exp) (eval (caddr exp) init-env) init-env))
      (format "~a~n" (eval exp init-env))))

(define (read-exps explist)
  (for ((e explist)) (read-exp e)))

(define prelude
 '((define square (lambda (x) (* x x)))
   (define factorial (lambda (n) (cond ((< n 2) n)(else (* n (factorial (- n 1)))))))
   (square 2)))

(define (alltraceon) (trace dynamic-eval lexical-eval eval lexical-apply dynamic-apply)); bind pair-up lookup evlist evcond))
(define (alltraceoff) (untrace dynamic-eval lexical-eval eval lexical-apply dynamic-apply bind pair-up lookup evlist evcond))
;now turn all tracing on so that we can watch evaluation at the repl
(alltraceoff)
(printf "reading the prelude...")
(read-exps prelude)
(printf "read~n")
(alltraceon)

(printf "try: ~a~n" '(eval '((λ (f) ((λ (x) (f)) 'dynamic)) ((λ (x) (λ () x)) 'lexical)) '()))
(printf "use (set! eval dynamic-eval) and (set! eval lexical-eval) to change between the two philosophies")
(printf "     (toplevel) for the REPL~n")

#|
(eval '((lambda (n) 
          ((lambda (fact) (fact fact n)) 
           (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1))))))))
        0) '((* . prim-times)(- . prim-minus)(< . prim-lessthan)))
|#


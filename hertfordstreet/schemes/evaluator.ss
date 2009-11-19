#lang scheme
(require scheme/mpair)
(require (lib "trace.ss"))

;using a global variable to control lexical vs dynamic binding
(define scope 'lexical)

(define (lexical-binding)
  (set! scope 'lexical)
  (printf "evaluating with lexical binding: ~n"))

(define (dynamic-binding)
  (set! scope 'dynamic)
  (printf "evaluating with dynamic binding: ~n"))

;the heart of the evaluator
(define (eval exp env)
  (cond ((number? exp) exp)
        ((eq? exp 'true) #t)
        ((eq? exp 'false) #f)
        ((eq? exp 'nil) '())
        ((symbol? exp) (lookup exp env))
        ((eq? (car exp) 'begin) (evbegin (cdr exp) env))
        ((eq? (car exp) 'define) (define-variable! (cadr exp) (eval (caddr exp) env) env))
        ((eq? (car exp) 'quote) (cadr exp))
        ((or (eq? (car exp) 'λ) (eq? (car exp) 'lambda)) 
         (list 'closure (cdr exp) env))
        ((eq? (car exp) 'cond) (evcond (cdr exp) env))
        (else (sapply (eval (car exp) env) (evlist (cdr exp) env) env))))

(define (sapply proc args env)
  (cond ((primitive? proc) (apply-primop proc args))
        ((eq? (car proc) 'closure) 
         (eval (cadadr proc) (bind (caadr proc) args (cond ((eq? scope 'lexical)(caddr proc))
                                                           ((eq? scope 'dynamic) env)
                                                           (else (error (format "scope (~a) is neither lexical nor dynamic" scope)))))))
        (else (error (format "apply error: trying to apply ~a to ~a in environment ~a" proc args env)))))

(define (evbegin sequence env)
  (cond ((null? sequence) (error "empty sequence in begin"))
        ((null? (cdr sequence)) (eval (car sequence) env))
        ((begin
           (eval (car sequence) env)
           (evbegin (cdr sequence) env)))))
      
(define (evlist explist env)
  (if (null? explist) '()
      (cons (eval (car explist) env) (evlist (cdr explist) env))))              

(define (evcond clauses env) 
  (cond ((null? clauses) (error "cond with no true clauses (missing else?)"))
        ((eq? (caar clauses) 'else)
         (eval (cadar clauses) env))
        ((false? (eval (caar clauses) env))
         (evcond (cdr clauses) env))
        (else (eval (cadar clauses) env))))


;environments
(define (bind vars vals env) (mcons (pair-up vars vals) env))

(define (pair-up vars vals)
  (cond
    ((eq? vars '()) (cond ((eq? vals '()) '())
                          (else (error 'TMA))))
    ((symbol? vars) (mcons (mcons vars vals) '()))
    ((eq? vals '()) (error 'TFA))
    (else
     (mcons (mcons (car vars)
                   (car vals))
            (pair-up (cdr vars)
                     (cdr vals))))))

(define (lookup sym env)
  (cond ((eq? env '()) (error (format "unbound variable: ~a" sym)))
        (else
         ((lambda (vcell)
            (cond ((eq? vcell '())
                   (lookup sym
                           (mcdr env)))
                  (else (mcdr vcell))))
          (new-assq sym (mcar env))))))

(define (new-assq sym alist)
  (cond ((eq? alist '()) '())
        ((eq? sym (mcar (mcar alist)))
         (mcar alist))
        (else (new-assq sym (mcdr alist)))))

(define (define-variable! var val env)
  (let ((frame (mcar env)))
    (let ((vcell (new-assq var frame)))
      (if (null? vcell)
          (let ((newvcell (mcons var val))
                (newlink (mcons (mcar frame) (mcdr frame))))
            (set-mcar! frame newvcell)
            (set-mcdr! frame newlink)
            (format "defined ~a" var))
          (begin (set-mcdr! vcell val) (format "mutated ~a!!" var))))))

;pre-defined primitive ops
(define (primitive? op) (or (eq? op 'prim-times)
                            (eq? op 'prim-plus)
                            (eq? op 'prim-equals)
                            (eq? op 'prim-car)
                            (eq? op 'prim-cdr)
                            (eq? op 'prim-cons)
                            (eq? op 'prim-minus)
                            (eq? op 'prim-lessthan)
                            (eq? op 'prim-eq?)
                            (eq? op 'prim-div)))
(define (apply-primop op args)
  (cond ((eq? op 'prim-times)  (apply * args))
        ((eq? op 'prim-plus) (apply + args))
        ((eq? op 'prim-equals) (apply = args))
        ((eq? op 'prim-car)    (apply car args))
        ((eq? op 'prim-cdr)    (apply cdr args))
        ((eq? op 'prim-cons)   (apply cons args))
        ((eq? op 'prim-lessthan)   (apply < args))
        ((eq? op 'prim-minus)  (apply - args))
        ((eq? op 'prim-eq?) (apply eq? args))
        ((eq? op 'prim-div) (apply / args))
        ))

(define init-env (bind '(* + = car cdr cons < - eq? /) '(prim-times prim-plus prim-equals prim-car prim-cdr prim-cons prim-lessthan prim-minus prim-eq? prim-div)'()))

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
   (failtest (lookup 'z (bind '(x y z) '(1 2 3) '())))
   (test (lookup 'z (bind '(x y z) '(1 2 3) '())) 4)
   (test (lookup 'w (bind '(x y z) '(1 2 3) '())) 3)
   ))

(let ((ttt (test-testing-tests)))
  (if (for/or ((i ttt)) (car i)) 
      (error (format "test-testing-tests are broken ~a" (map cdr (filter car ttt))))
      "tests tested successfully"))

(define xyz123env (bind '(x y z) '(1 2 3) '()))

(define (environmenttests)
  (list
   (test     (lookup 'z xyz123env) 3)
   (failtest (lookup 'w xyz123env))
   (test     (map (λ (q) (lookup q (bind '(w z) '(20 50) xyz123env))) '(x y z w)) '(1 2 50 20))
   (failtest (lookup 'u (bind '(w z) '(20 50) xyz123env)))
   (test     (map (λ (x) (lookup x (bind '() '() (bind '(x z) '(10 2) (bind '(x . y) '(1 2 3 4) init-env))))) '(x z y * =))
             '(10 2 (2 3 4) prim-times prim-equals))
   (failtest (bind '(x) '(1 2) init-env))
   (failtest (bind '(x y) '(1) init-env))))

(define (environmentchangetests)
  (let* ((littleenv (bind '(y z) '(10 20) (bind '(z w) '(100 200) '())))
         (bigenv (bind '(x y) '(1 2) littleenv)))
    (define-variable! 'x 1000 bigenv)
    (define-variable! 'u 2000 bigenv)
    (define-variable! 'z 3000 bigenv)
    (list
     (test (lookup 'x bigenv) 1000)
     (test (map (λ(x) (lookup x bigenv)) '(x y z w u)) '(1000 2 3000 200 2000))
     (test (map (λ(x) (lookup x littleenv)) '(y z w)) '(10 20 200))
     (failtest (lookup 'a bigenv)) 
     )))

(define (applytests)
  (list
   (test     (sapply `(closure ((x) (* x x)) ,init-env) '(2) init-env) 4)
   (failtest (sapply '(lambda (x) (* x x)) '(2) init-env))
   ))

(define (evaltests)
  (list  
   (test     (eval '2 '()) 2)
   (test     (eval '(begin 1 2) init-env) 2)
   (test     (eval '(quote x) '()) 'x)
   (test     (eval 'x (bind 'x '4 '())) 4)
   (test     (eval '((λ (x) (* x x)) 2) init-env) 4)
   (test     (eval '((lambda (x) (* x x)) 2) init-env) 4)
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
   (test     (eval '((λ x (* (car x) (car (cdr x)))) 3 4) init-env) 12)
   (test     (eval '((λ () (* x (* (car y) (car (cdr y)))))) (bind '(x y) '(10 (2 3)) init-env)) 60)
   (test     (eval '((lambda(x) (cons (cdr x)(car x))) (cons 'a 'b)) init-env) '(b . a))
   (test (eval '((lambda (list) (list 1 2 3))(lambda l l)) init-env) '(1 2 3))
   (test (eval '((lambda (n) 
          ((lambda (fact) (fact fact n)) 
           (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1))))))))
        10) init-env) 3628800)
   (test (eval '(cond (true  'hi)) init-env) 'hi)
   (failtest (eval '(cond (false 'hi)) init-env))
   ))

(define (dynamic-binding-tests)
  (list
   (test     (eval '((λ (p y) (p 5)) (λ (x) (* x y)) 10) init-env) 50)
   (test     (eval '((λ (p y) (p 5)) (λ (x) (* x y)) 10) init-env) 50)
   (failtest (eval '(((λ (y) (λ (x) (* y x))) 2) 3) init-env))
   (test (eval '((λ (p y) (p 5)) ((λ (y) (λ (x) (* x y))) 100) 10) init-env) 50)
   (test (eval '((λ (f)   (* ((λ (x) (f)) 3)((λ (x) (f)) 4))) ((λ (x) (λ () x)) 2)) init-env) 12)
   (test (eval '((λ (f) ((λ (x) (f)) 'dynamic)) ((λ (x) (λ () x)) 'lexical)) init-env) 'dynamic)))

(define (lexical-binding-tests)
  (list
   (failtest     (eval '((λ (p y) (p 5)) (λ (x) (* x y)) 10) init-env))
   (failtest     (eval '((λ (p y) (p 5)) (λ (x) (* x y)) 10) init-env))
   (test (eval '(((λ (y) (λ (x) (* y x))) 2) 3) init-env) 6 )
   (test (eval '((λ (p y) (p 5)) ((λ (y) (λ (x) (* x y))) 100) 10) init-env) 500)
   (test (eval '((λ (f)   (* ((λ (x) (f)) 3)((λ (x) (f)) 4))) ((λ (x) (λ () x)) 2)) init-env) 4)
   (test (eval '((λ (f) ((λ (x) (f)) 'dynamic)) ((λ (x) (λ () x)) 'lexical)) init-env) 'lexical)
   ))

(define (dotests . testprocs)
  (let* ((t (append-map (λ(p)(p)) testprocs ))
         (success (for/and ((i t)) (car i)))
         (failingtests (map (λ(s) (cadr s))(filter (λ(s) (not (car s))) t))))
    (if success "all tests pass" (list "!!!!! TEST FAILURE!!!!!->" failingtests))))

;do all the tests with dynamic binding
(dynamic-binding)
(dotests environmentchangetests environmenttests evaltests applytests dynamic-binding-tests)

;do them with lexical binding
(lexical-binding)
(dotests environmentchangetests environmenttests evaltests applytests lexical-binding-tests)

;set lexical binding for the rest of the program
(lexical-binding)

;now start reading expressions into the initial environment
(define (read-exp exp) (eval exp init-env))

(define (read-exps explist)
  (for ((e explist)) (print (read-exp e))))

;here are some predefined functions that we can add in our new language
(define prelude
  '((define square (lambda (x) (* x x)))
    (define factorial (lambda (n) (cond ((< n 2) n)(else (* n (factorial (- n 1)))))))
    (define not (lambda (x) (cond (x false) (else true))))
    (define even? (lambda (x) (cond ((= x 0) true)  (else (odd? (- x 1))))))
    (define odd?  (lambda (x) (cond ((= x 0) false) (else (even? (- x 1))))))
    (define map (lambda (f lst) (cond ((eq? lst nil) nil) (else (cons (f (car lst)) (map f (cdr lst)))))))
    (define seq (lambda (a b) (cond ((= a b)(cons a nil)) (else (cons a (seq (+ a 1) b))))))
    (define sqrt (lambda (x)
                  (begin
                    (define abs (lambda (x) (cond ((< x 0)(- x))(else x))))
                    (define average (lambda (a b) (/ (+ a b) 2)))
                    (define improve (lambda (guess) (average guess (/ x guess))))
                    (define good-enough? (lambda (guess) (< (abs (- (square guess) x)) .001)))
                    (define try (lambda (guess) (cond ((good-enough? guess) guess) (else (try (improve guess))))))
                    (try 1))))))

(define (preludetests)
  (list (test (eval '(map square (seq 0 10)) init-env) '(0 1 4 9 16 25 36 49 64 81 100))
        (test (eval '(map factorial (seq 0 10)) init-env) '(0 1 2 6 24 120 720 5040 40320 362880 3628800))
        (test (eval '(map even? (seq 0 10)) init-env) '(#t #f #t #f #t #f #t #f #t #f #t))
        (test (eval '(map odd? (seq 0 10)) init-env) '(#f #t #f #t #f #t #f #t #f #t #f))
        (test (eval '(not false) init-env) #t)
        (test (eval '(sqrt 4) init-env) 21523361/10761680)
        ))

(printf "reading the prelude...")
(read-exps prelude)
(printf "read~n")
(printf "testing the prelude...")
(dotests preludetests)

;now go interactive 
(define (toplevel)
  (let ((entered (read)))
        (printf "~a~n" (eval entered init-env))
    (toplevel)))

(define (alltraceon) (trace  eval sapply)); bind pair-up lookup evlist evcond))
(define (alltraceoff) (untrace  eval sapply bind pair-up lookup evlist evcond))
;now turn all tracing on so that we can watch evaluation at the repl
(alltraceoff)
(alltraceon)

(printf "try: ~a~n" '(eval '((λ (f) ((λ (x) (f)) 'dynamic)) ((λ (x) (λ () x)) 'lexical)) '()))
(printf "use (lexical-binding) and (dynamic-binding) to change between the two philosophies")
(printf "     (toplevel) for the REPL~n")



 





























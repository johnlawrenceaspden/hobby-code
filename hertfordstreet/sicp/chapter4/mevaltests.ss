#lang scheme

(require "bookevaluator.ss")

;;test macros

(define-syntax cleantests
  (syntax-rules (->)
    ((_ name (a -> b) ...) (define (name) (list (cleantest a b) ...)))))

(define-syntax cleanfailtests
  (syntax-rules ()
    ((_ name a ...) (define (name) (list (cleanfailtest a) ...)))))

(define-syntax testsequence ;relying on plt's left-to-right guarantee here...
  (syntax-rules (->)
    ((_ name (a -> b) ...) (define (name)
                             (let ((testenv (setup-environment)))
                               (list 
                                (test-in-environment testenv a b) ...))))))

(define-syntax-rule (cleantest a b) (test-in-environment (setup-environment) a b))
(define-syntax-rule (cleanfailtest a) (failtest-in-environment (setup-environment) a))

(define-syntax-rule (test-in-environment env a b) (test (meval (quote a) env) b))
(define-syntax-rule (failtest-in-environment env a) (failtest (meval (quote a) env)))
;(define-syntax-rule (test-in-environment env a b) (test ((analyse (quote a)) env) b))
;(define-syntax-rule (failtest-in-environment env a) (failtest ((analyse (quote a)) env)))



(define-syntax-rule (test expression expected-result)
  (let ((expstring (format "~a" (quote expression))))
    (with-handlers ((exn:meval? (λ(v) (list #f (format "~a threw [~a]" expstring v))))
                    (exn:fail? (λ(v)  (list #f (format "~a blew up with [~a]" expstring v)))))
      (let* ((result expression)
             (success (equal? result expected-result)))
        (list success (format "~a -> ~a (should be ~a)" expstring result expected-result))))))

(define-syntax-rule (failtest expression)
  (let ((expstring (format "~a" (quote expression))))
    (with-handlers ((exn:meval? (λ(v) (list #t (format "~a correctly threw [~a]" expstring v))))
                    (exn:fail?  (λ(v) (list #f (format "~a blew up with [~a]" expstring v)))))    
      (let ((result expression))
        (list #f (format "~a should have thrown exception, but completed, returning ~a" expstring result))))))

(define-syntax-rule (dotests testprocs ... )
  (let* ([testproclist (list testprocs ...)]
         [t (append-map (λ(p)(p)) testproclist )]
         [numtests (length t)]
         [passingtests (map (λ(s) (cadr s)) (filter (λ(s) (car s)) t))]
         [failingtests (map (λ(s) (cadr s)) (filter (λ(s) (not (car s))) t))]
         [numpass (length passingtests)]
         [numfail (length failingtests)]
         )
    (cond 
      [(not (= (+ numpass numfail) numtests)) (error "eek!")]
      [(= numpass numtests) (format "~a all ~a tests pass~n" testproclist numtests )]
      [(= numfail numtests) (format "~a all ~a tests fail~n" testproclist numtests )]
      [else (list (format "~a !!!!! TEST FAILURES!!!!! (~a of ~a)->" testproclist numfail numtests) failingtests)])))


;the tests

(cleantests evaltests
            ( false -> #f )
            ( true -> #t )
            ( 2 -> 2 )
            ( "string" -> "string" )
            ( (begin 1 2) -> 2 )
            ( 'x -> 'x )
            ( (if false 'a) -> #f )
            ( ((λ (x) (* x x)) 2) -> 4 )
            ( ((lambda (x) (* x x)) 2) -> 4 )
            ( (cond (true 'hi)) -> 'hi )
            ( (cond (false 'hi)) -> #f )
            ( ((lambda (x) (cond ((= x 1) 'zero 'one) ((= x 2) 'two) (else 'other))) 1) -> 'one )
            ( ((lambda (x) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) 2) -> 'two )
            ( ((lambda (x) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) 3) -> 'other )
            ( ((lambda (x) (cons (cdr x) (car x))) (cons 'a 'b)) -> (cons 'b 'a) )
            ( ((lambda (n) ((lambda (fact) (fact fact n)) (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))))) 10 ) -> (* 1 2 3 4 5 6 7 8 9 10) ))


(testsequence definition-tests
              ( (define square (lambda (x) (* x x x x))) -> 'ok )
              ( (define x 2) -> 'ok )
              ( (square x) -> 16 )
              ( (define (square x) (* x x x)) -> 'ok )
              ( (square x) -> 8 )
              ( (define (square x) (* x x)) -> 'ok )
              ( (square x) -> 4 )
              ( (define (map f l) (cond ((null? l) '())(else (cons (f (car l)) (map f (cdr l)))))) -> 'ok )
              ( (map square '(1 2 3)) -> '(1 4 9)) )

(testsequence assignment-tests
              ( ((lambda(a) (set! a 3) a) 2) -> '3 )
              ( (define global 2) -> 'ok )
              ( ((lambda(a) (set! global 3) a) 2) -> 2 ))

(cleanfailtests error-catching-tests
                ((lambda (x) (* x x)) 1 2) 
                ((lambda (x) (* x x)) )
                ((lambda x (* (car x) (car x))))
                ()
                (cond (()))
                (cond (false 'a)(else 'c)(false 'b))
                (cond (else))
                (cond 1)
                (cond (1 2) '())
                (false 1)
                (non-existent-global)
                (set! non-existent-global)
                (set! non-existent-global 5)
                (/ 1 0)
                (begin (define (weird) (define x x) x) (weird)))

(printf "meta-evaluator's evaluation order is ~a~n" (meval '((lambda (a) (cons (set! a 'right-to-left) (set! a 'left-to-right)) a) false) (setup-environment)))
(printf "analyser's evaluation order is ~a~n" ((analyse '((lambda (a) (cons (set! a 'right-to-left) (set! a 'left-to-right)) a) false)) (setup-environment)))
(printf "the order in the underlying scheme is ~a~n" ((lambda (a) (cons (set! a 'right-to-left) (set! a 'left-to-right)) a) false))
(printf "the order in the underlying scheme via eval is ~a~n" (eval '((lambda (a) (cons (set! a 'right-to-left) (set! a 'left-to-right)) a) #f) (make-base-namespace)))

;the original evaluator in the book should pass tests up to here
;the following test extra features added in exercises.

(cleantests and-or-tests
            ( (and) -> #t )
            ( (and true) -> #t )
            ( (and 1) -> 1 )
            ( (and false (/ 1 0)) -> #f)
            ( (and true true true false (/ 1 0)) -> #f)
            ( (or) -> #f )
            ( (or true) -> #t )
            ( (or 1) -> 1 )
            ( (or true (/ 1 0)) -> #t)
            ( (or true true true false (/ 1 0)) -> #t)
            ( (or false false false true (/ 1 0)) -> #t)          
            )

(testsequence extended-cond-tests
              ( (cond) -> #f )
              ( (cond [(= 0 1) (/ 0 0)][(< 2 0)(/ 0 0)][5]) -> 5 )
              ( (define (cdar x) (cdr (car x))) -> 'ok )
              ( (define (caar x) (car (car x))) -> 'ok )
              ( (define (explode) (/ 0 0)) -> 'ok )
              ( (define (assq a alist) (if (null? alist) false (if (eq? (caar alist) a) alist (assq a (cdr alist))))) -> 'ok )
              ( (define mylist '((a . 1)(b . 2))) -> 'ok )
              ( (cond 
                  ((assq 'c mylist) => cdar)
                  ((assq 'b mylist) => cdar)
                  (else explode)) -> 2 )
              ( ((lambda (var) (cond ((begin (set! var (+ var 1)) var) => (lambda(x) x)))) 0) -> 1)
              )

(cleantests let-tests
            ( (let ([x 5]) x) -> 5)
            ( (let ([b 2][a 1]) (cons a b)) -> '(1 . 2))
            ( (let ([a 1][b 2][c 3]) (cons a (cons b (cons c '())))) -> '(1 2 3))
            ( (let ([x 5]) (let ([x 2][y x]) (cons y (cons x '())))) -> '(5 2))
            ( (let ((x 2)) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) -> 'two )
            ( (let ((x 3)) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) -> 'other )
            ( (let () 'a) -> 'a )
            ( (let ([me "Tarzan"][you "Jane"])(let ([me you][you me])(cons me you))) -> '("Jane" . "Tarzan")))

(cleanfailtests let-fail-tests
                (let ((a)(b 2)) 'doom)
                (let (()))
                (let ((a 1)))
                (let ((a 1 b 2)))
                (let (()) body)
                ;(let ((me "Robert")(me "Bob")) me) plt catches this, but my little evaluator doesn't, and checking for it might slow things down.
                )

(define (let*-tests)
  (list 
   (cleantest (let* ((a 1)(b 2)(c 3)) (cons a (cons b (cons c '())))) '(1 2 3))
   (cleantest (let* ((a 1)(b a)(c (+ a b))) (cons a (cons b (cons c '())))) '(1 1 2))
   (cleantest (let* ((a 1)(b 2)(c 3)) (set! a c) (cons a (cons b (cons c '())))) '(3 2 3))
   (cleantest (let* ((a 1)(b 2)(c a)) (set! c b) (cons a (cons b (cons c '())))) '(1 2 2))
   (cleantest (let* () 'a) 'a)
   (cleantest (let* () 'a 'b) 'b)
   (cleanfailtest (let* ((b a)(a 1)) (cons b a)))
   (cleanfailtest (let* ((a 1)(b 1))))
   (cleanfailtest (let* (()) 'a))
   (cleanfailtest (let* ((a 1)(b)(c (+ a b))) (cons a (cons b (cons c '())))))
   (cleantest     (let* ((a 1)(b a)(c (+ a b))) (cons a (cons b (cons c '())))) '(1 1 2))
   (cleantest (let* ([name (cons "Burroughs" '())][name (cons "Rice" name)][name (cons "Edgar" name)]) name) '("Edgar" "Rice" "Burroughs"))
   (cleanfailtest (let* ((a b)(b 1)) (cons a b)))))

(cleantests named-let-tests
            ( (let fac ([n 10]) (if (= n 0) 1 (* n (fac (- n 1))))) -> (* 10 9 8 7 6 5 4 3 2 1))
            ( (let fib ((n 10)) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) -> 55 ))

(cleantests variable-argument-list-tests
            ( ((λ y (* (car y) (car (cdr y)) (car (cdr (cdr y))))) 1 2 3 4) -> 6 )
            ( ((λ x x ) 1 2 3 4 5) -> '(1 2 3 4 5) )
            ( ((λ x (* (car x) (car (cdr x)))) 3 4) -> 12 )
            ( ((lambda (list) (list 1 2 3)) (lambda l l)) -> '(1 2 3)  ) 
            ( ((λ (x . y) (* x (* (car y) (car (cdr y))))) 1 2 3 4) -> 6 )
            ( ((λ (x . y) (set! x 10)(set! y '(20 30 40))(* x (* (car y) (car (cdr y)) (car (cdr (cdr y)))))) 1 2) -> 240000 ))

(testsequence silly-iteration-construct-tests
              ( (define (even? x) (if (= x 0) true  (odd?  (- x 1)))) -> 'ok )
              ( (define (odd?  x) (if (= x 0) false (even? (- x 1)))) -> 'ok )
              ( (list/c (* x x) for x in '(1 2 3 4 5 6 7 8 9 10) if (odd? x)) -> '(1 9 25 49 81))
              ( (list/c x for x in (list/c (* x x) for x in '(1 2 3 4 5 6 7 8 9 10)) if (odd? x)) -> '(1 9 25 49 81))
              ( (list/c (list/c (* x y) for x in '(1 2 3 4)) for y in '(3 4 5)) -> '((3 6 9 12) (4 8 12 16) (5 10 15 20)))
              ( (list/c (* x y) for x in '(1 2 3 4) for y in '(3 4 5) if (= x y)) -> '(9 16) )
              ( (list/c (list/c (/ x y) for x in '(1 2 3) if (< x y)) for y in '(1 2 3) ) -> '(()(1/2)(1/3 2/3))))

(testsequence internal-definitions-tests
              ( (define (evenfilter l)
                  (define (even? x)
                    (define (odd? x) (if (= x 0) false (even? (- x 1))))
                    (if (= x 0) true  (odd?  (- x 1))))
                  (define (filter f l)
                    (if (null? l) '()
                        (if (f (car l)) (cons (car l) (filter f (cdr l)))
                            (filter f (cdr l)))))
                  (filter even? l)) -> 'ok )
              ( (evenfilter '(1 2 3 4 5 6 7 8 9)) -> '(2 4 6 8)))




(dotests evaltests definition-tests assignment-tests error-catching-tests and-or-tests extended-cond-tests let-tests let-fail-tests let*-tests named-let-tests variable-argument-list-tests silly-iteration-construct-tests internal-definitions-tests )
(printf "should work up to here at least:~nafter here are tests for unimplemented features~n")






;(dotests )

;(provide all-defined-out)








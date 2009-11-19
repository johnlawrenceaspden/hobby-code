#lang scheme

(require "bookevaluator.ss")
(require "merror.ss")
(require scheme/help)

;(define mylist '((a . 1)(b . 2)))
;(assq 'b mylist)



;examples of single tests that can be run from the command line
(define (command-line-tests)
  (list 
   (perform-test 'single '((/ 100 0)))
   (perform-test 'single '((/ 100 0) is an error))
   (perform-test 'single '((* 3 5) -> 15))
   (perform-test-set ouch-differences-between-the-evaluators)
   (call-with-values (λ()(interpret 'dummy (perform-test-set ouch-differences-between-the-evaluators))) list)))


;                                                                                       
;                                                                                       
;                              ;;                                                 ;     
;   ;                 ;       ;                                                   ;     
;   ;                 ;       ;                                                   ;     
;  ;;;   ;;;    ;;;  ;;;     ;;; ; ;  ;;;   ; ;;  ;;    ;;;  ;   ;   ;  ;;;   ; ; ;   ; 
;   ;   ;   ;  ;   ;  ;       ;  ;   ;   ;  ;;  ;   ;  ;   ; ;   ;   ; ;   ;  ;   ;  ;  
;   ;   ;   ;  ;;     ;       ;  ;       ;  ;   ;   ;  ;   ;  ; ;;; ;  ;   ;  ;   ; ;   
;   ;   ;;;;;   ;;;   ;       ;  ;     ;;;  ;   ;   ;  ;;;;;  ; ; ; ;  ;   ;  ;   ;;;   
;   ;   ;          ;  ;       ;  ;   ;;  ;  ;   ;   ;  ;      ; ; ;;;  ;   ;  ;   ;  ;  
;   ;   ;   ;  ;   ;  ;       ;  ;   ;  ;;  ;   ;   ;  ;   ;   ;   ;   ;   ;  ;   ;  ;  
;   ;;   ;;;    ;;;   ;;      ;  ;    ;; ;  ;   ;   ;   ;;;    ;   ;    ;;;   ;   ;   ; 
;                                                                                       
;                                                                                       
;                                                                                 ;     


;given a piece of code and three environments in which to execute it, return a threeway, 
;containing results and timing information for the three methods of evaluation
(define-struct timeresults (result cputime realtime gctime) #:transparent)
(define-struct threeway (testsection code plt meval analysis execution) #:transparent)

(define (threewayeval testsection testcode pltenvironment mevalenvironment analyseenvironment)
  (let ([plt       (with-handlers((exn:fail? (λ(v)v)))(call-with-values (λ()(time-apply (λ()(eval testcode pltenvironment)) '())) make-timeresults)) ]
        [meval     (with-handlers((exn:meval? (λ(v)v)))(call-with-values (λ()(time-apply (λ()(meval testcode mevalenvironment)) '())) make-timeresults)) ]
        [analysis  (with-handlers((exn:meval? (λ(v)v)))(call-with-values (λ()(time-apply (λ()(analyse testcode)) '())) make-timeresults))])
    (if (exn:meval? analysis)
        (make-threeway testsection testcode plt meval analysis 'error-in-analysis-could-not-execute)
        (let* ([executionprocedure (car (timeresults-result analysis))]
               [execution (with-handlers((exn:meval? (λ(v)v)))(call-with-values (λ()(time-apply (λ()(executionprocedure analyseenvironment)) '())) make-timeresults))])
          (make-threeway testsection testcode plt meval analysis execution)))))



;given a threeway, determine whether the results were as expected

(define-struct testresult  (success comment) #:transparent)

(define (assert-no-error t)
  (let ([plterr (exn:fail? (threeway-plt t))]
        [mevalerr (exn:meval? (threeway-meval t))]
        [analyseerr (or (exn:meval? (threeway-analysis t))(exn:meval? (threeway-execution t)))])
    (if (or plterr mevalerr analyseerr)
        (make-testresult #f (format "~a unexpected failure  (detailed results ~a)" (threeway-code t) t ))
        (make-testresult #t 'all-succeeded-as-expected))))

(define (assert-error t)
  (let ([plterr (exn:fail? (threeway-plt t))]
        [mevalerr (exn:meval? (threeway-meval t))]
        [analyseerr (or (exn:meval? (threeway-analysis t))(exn:meval? (threeway-execution t)))])
    (if (and plterr mevalerr analyseerr)
        (make-testresult #t 'all-failed-as-expected)
        (make-testresult #f (format "~a unexpected success (detailed results ~a)" (threeway-code t) t )))))

(define (assert-result t expected)
  (let ([plterr (exn:fail? (threeway-plt t))]
        [mevalerr (exn:meval? (threeway-meval t))]
        [analyseerr (or (exn:meval? (threeway-analysis t))(exn:meval? (threeway-execution t)))])
    (if (or plterr mevalerr analyseerr)
        (make-testresult #f (format "~a -> ~a unexpected error (detailed results ~a)" (threeway-code t) expected t))
        (let ([plt-results (timeresults-result (threeway-plt t))]
              [meval-results (timeresults-result (threeway-meval t))]
              [analyse-results (timeresults-result (threeway-execution t))])
          (if (and (equal? plt-results (list expected))
                   (equal? meval-results (list expected))
                   (equal? analyse-results (list expected)))
              (make-testresult #t 'all-as-expected)
              (make-testresult #f (format "~a -> ~a unexpected results: (~a ~a ~a)" (threeway-code t) expected plt-results meval-results analyse-results)))))))

(define (assert-result-in-meta-error-in-plt t expected)
  (let ([plterr (exn:fail? (threeway-plt t))]
        [mevalerr (exn:meval? (threeway-meval t))]
        [analyseerr (or (exn:meval? (threeway-analysis t))(exn:meval? (threeway-execution t)))])
    (if (or mevalerr analyseerr)
        (make-testresult #f (format "~a -> ~a (but error in plt) ~n unexpected error in one of ~nmeval: ~a ~nanalyse: ~a ~nexecution ~a ~n" (threeway-code t) expected (threeway-meval t) (threeway-analysis t) (threeway-execution t)))
        (if (not plterr)
            (make-testresult #f (format "~a -> ~a plt did not fail (detailed results ~a)" (threeway-code t) expected t))
            (let ([meval-results (timeresults-result (threeway-meval t))]
                  [analyse-results (timeresults-result (threeway-execution t))])
              (if (and (equal? meval-results (list expected))
                       (equal? analyse-results (list expected)))
                  (make-testresult #t 'all-as-expected)
                  (make-testresult #f (format "~a -> ~a unexpected results: (~a ~a)" (threeway-code t) expected meval-results analyse-results))))))))


;given an expression and the environments in which to analyse it, return a pair containing the raw data and the analysis
(define (check-correct testsection expression expected-result plt-environment meval-environment analyse-environment)
  (let ([results (threewayeval testsection expression plt-environment meval-environment analyse-environment)])
    (cons (assert-result results expected-result) results)))

(define (check-correct-in-meta-error-in-plt testsection expression expected-result plt-environment meval-environment analyse-environment)
  (let ([results (threewayeval testsection expression plt-environment meval-environment analyse-environment)])
    (cons (assert-result-in-meta-error-in-plt results expected-result) results)))

(define (check-error testsection expression plt-environment meval-environment analyse-environment)
  (let ([results (threewayeval testsection expression plt-environment meval-environment analyse-environment)])
    (cons (assert-error results) results)))

(define (check-no-error testsection expression plt-environment meval-environment analyse-environment)
  (let ([results (threewayeval testsection expression plt-environment meval-environment analyse-environment)])
    (cons (assert-no-error results) results)))


;an evaluator for the test set language
;every expression returns a list of results, which are then flattened out to produce one big list per test set

(define (perform-test-set testset)
  (append-map (λ(test) (perform-test (list (car testset)) test)) (cdr testset)))

;each individual test is done in its own freshly created environment
(define (perform-test testsection test)
  (let ([plt-env     (make-base-namespace)]
        [meval-env   (setup-environment)]
        [analyse-env (setup-environment)])
    (match test
      ((list a '-> b) (list (check-correct testsection a b plt-env meval-env analyse-env)))
      ((list a '-> b 'but 'error 'in 'plt) (list (check-correct-in-meta-error-in-plt testsection a b plt-env meval-env analyse-env)))
      ((list a 'is 'an 'error) (list (check-error testsection a plt-env meval-env analyse-env)))
      ((list a) (list (check-no-error testsection a plt-env meval-env analyse-env)))
      ((list 'sequence (? symbol? name) a ...) (append-map (λ(test) (perform-test-in-environments (cons name testsection) test plt-env meval-env analyse-env)) a))
      ((list 'subsection (? symbol? name) a ...) (append-map (λ(test) (perform-test (cons name testsection) test)) a ))
      (a (error (format "unknown test expression ~a" a)))) 
    ))

(define (perform-test-in-environments testsection test plt-env meval-env analyse-env)
  (match test
    ((list a '-> b) (list (check-correct testsection a b plt-env meval-env analyse-env)))
    ((list a '-> b 'but 'error 'in 'plt) (list (check-correct-in-meta-error-in-plt testsection a b plt-env meval-env analyse-env)))
    ((list a 'is 'an 'error) (list (check-error testsection a plt-env meval-env analyse-env)))
    ((list a) (list (check-no-error testsection a plt-env meval-env analyse-env)))
    ((list 'sequence a ...)    (error "nested sequences not allowed"))
    ((list 'subsection a ...) (error "subsections not allowed in sequences"))
    (a (error (format "unknown test expression (in sequence) ~a" a)))))

;interprets the rather inscrutable test result format for human eyes
;expecting a list of (testresult . threeway) pairs
(define (comment-list testsetname passed failed)
  (if (= (length failed) 0)
      (cond ((= (length passed) 0) (list (format "~a: no tests!!" testsetname)))
            ((= (length passed) 1) (list (format "~a: only one test, which passed" testsetname)))
            ((= (length passed) 2) (list (format "~a: both passed" testsetname)))
            (else (list (format "~a: all ~a pass" testsetname (length passed)))))
      (cons (format "~a: ~a passes ~a fails" testsetname (length passed) (length failed))
            (map (λ(f) 
                   (let ([section-name (string-join (map symbol->string (reverse (threeway-testsection (cdr f)))) "/")])
                     (format "fail:~a: ~a" section-name (testresult-comment (car f)))))
                 failed))))


(define (interpret testsetname testresults)
  (let-values ([(sane oops)  (partition (λ(x) (and (pair? x) (testresult? (car x)))) testresults)])
    (if (> (length oops) 0)
        (error (format "something screwy is going on: ~a" oops))
        (let-values ([(passed failed) (partition (λ(x) (testresult-success (car x))) sane)])
          (values (cons (length passed) (length failed)) (comment-list testsetname passed failed))
          ))))

;macro to make the tests look a bit nicer
(define-syntax define-test-set 
  (syntax-rules ()
    ((define-test-set name body ...)
     (define name (cons (quote name) (quasiquote (body ...)))))))



;                                                                                                    
;                                                                                                    
;                                                   ;                                                
;   ;                 ;       ;                 ;                        ;                 ;         
;   ;                 ;       ;                 ;                        ;                 ;         
;  ;;;   ;;;    ;;;  ;;;     ;;;   ;;;    ;;;  ;;;  ;  ; ;;    ;; ;     ;;;   ;;;    ;;;  ;;;   ;;;  
;   ;   ;   ;  ;   ;  ;       ;   ;   ;  ;   ;  ;   ;  ;;  ;  ;  ;;      ;   ;   ;  ;   ;  ;   ;   ; 
;   ;   ;   ;  ;;     ;       ;   ;   ;  ;;     ;   ;  ;   ;  ;   ;      ;   ;   ;  ;;     ;   ;;    
;   ;   ;;;;;   ;;;   ;       ;   ;;;;;   ;;;   ;   ;  ;   ;  ;   ;      ;   ;;;;;   ;;;   ;    ;;;  
;   ;   ;          ;  ;       ;   ;          ;  ;   ;  ;   ;  ;   ;      ;   ;          ;  ;       ; 
;   ;   ;   ;  ;   ;  ;       ;   ;   ;  ;   ;  ;   ;  ;   ;  ;  ;;      ;   ;   ;  ;   ;  ;   ;   ; 
;   ;;   ;;;    ;;;   ;;      ;;   ;;;    ;;;   ;;  ;  ;   ;   ;; ;      ;;   ;;;    ;;;   ;;   ;;;  
;                                                                 ;                                  
;                                                             ;  ;;                                  
;                                                              ;;;                                   


;examples of the test set language, which set up tests which fail. These are meant to test the test routines themselves!
(define-test-set deliberatefailures
  ( ( lambda (a b)) -> 'ouch )
  ( 1 -> 2 )
  ( 1 is an error )
  ( 1 -> 1 but error in plt )
  ( (/ 1 0) -> 2 but error in plt )
  ( (/ 1 0) )
  ( true -> #f but error in plt )
  ( subsection a-short-subsection
               ( x -> 2 )
               ( x -> 3 ))
  ( sequence a-sequence
             ( (define a 1) -> ok )
             ( a -> 2 )
             ))

(define-test-set onlyonetest
  ( a is an error ))

(define-test-set emptytestset)

;this runs the deliberate failure test sets, and should give the test code a workout for code coverage purposes
(define (extracodecoverage)
  (command-line-tests)
  (for/list ((testset (list deliberatefailures onlyonetest emptytestset)))
    (call-with-values (λ()(interpret (car testset) (perform-test-set testset))) list)))


;                                                                                      
;                                                                                      
;                      ;                                                               
;                      ;                ;                  ;                 ;         
;                      ;                ;                  ;                 ;         
;    ;;;  ;   ;  ;;;   ;  ;   ;   ;;;  ;;;   ;;;   ; ;    ;;;   ;;;    ;;;  ;;;   ;;;  
;   ;   ; ;   ; ;   ;  ;  ;   ;  ;   ;  ;   ;   ;  ;       ;   ;   ;  ;   ;  ;   ;   ; 
;   ;   ;  ; ;      ;  ;  ;   ;      ;  ;   ;   ;  ;       ;   ;   ;  ;;     ;   ;;    
;   ;;;;;  ; ;    ;;;  ;  ;   ;    ;;;  ;   ;   ;  ;       ;   ;;;;;   ;;;   ;    ;;;  
;   ;      ;;;  ;;  ;  ;  ;   ;  ;;  ;  ;   ;   ;  ;       ;   ;          ;  ;       ; 
;   ;   ;   ;   ;  ;;  ;  ;  ;;  ;  ;;  ;   ;   ;  ;       ;   ;   ;  ;   ;  ;   ;   ; 
;    ;;;    ;    ;; ;  ;   ;; ;   ;; ;  ;;   ;;;   ;       ;;   ;;;    ;;;   ;;   ;;;  
;                                                                                      
;                                                                                      
;                                                                                      

(define-test-set axiom-tests
  (subsection self-evaluating-expressions
              ( #f -> #f )
              ( #t -> #t )
              ( 2  -> 2  )
              ( 2.0 -> 2.0 )
              ( 2/3 -> 2/3 )
              ( "hello" -> "hello" )
              ( () is an error )
              ( (#f 1) is an error )
              )
  (subsection sequences
              ( (begin) )
              ( (begin 1) -> 1 )
              ( (begin 1 2) -> 2 )
              ( (begin 1 2 3) -> 3)) 
  (subsection quotation             
              ( (quote a) -> a )
              ( 'quote -> quote )
              ( ''quote -> 'quote ))
  ( '() -> () )
  (subsection if
              ;( (if #f 'a) )   ;ok in metas
              ;( (if #f 'a ) is an error ) ;ok in plt
              ( (if #t 'a 'b) -> a )
              ( (if #f 'a 'b) -> b )
              ( (if #t 100 (/ 100 0)) -> 100 )
              ( (if #f 100 (/ 100 0)) is an error ))
  (sequence definition-lookup-assignment
            ( x is an error )
            ( (define x 1) )
            ( x -> 1 )
            ( (define x 2) )
            ( x -> 2 )
            ( (define y 10) )
            ( y -> 10 )
            ( x -> 2 )
            ( (set! x 20) )
            ( x -> 20 )
            ( y -> 10 )
            ( (set! y) is an error )
            ( z is an error )
            ( (set! z) is an error )
            ( (set! z 100) is an error )
            ( (define z 1000) )
            ( z -> 1000 )
            ( (set! z 2000) )
            ( z -> 2000 ))
  (subsection lambda
              ( ((lambda (x) x) "hello") -> "hello" )
              ( ((lambda (x) (if x 'ok 'oops)) #t) -> ok)
              ( ((lambda (a b) "ignore" 'this (if a (if b 'ok 'oops1) 'oops2)) #t #t) -> ok )
              ( ((lambda (a b) "ignore" 'this (if a (if b 'ok 'oops1) 'oops2)) #t #f) -> oops1 )
              ( ((lambda (a b) "ignore" 'this (if a (if b 'ok 'oops1) 'oops2)) #f #t) -> oops2 )
              ( ((lambda (x)) 2 ) is an error)
              ( ((lambda (x))) is an error  )
              ( ((lambda (x) x) 1 2) is an error )
              ))

(define-test-set variable-argument-list-tests
  ( ((lambda x x)) -> ())
  ( ((lambda x x) 1) -> (1))
  ( ((lambda x x) 1 2 3) -> (1 2 3))
  ( ((lambda (x . y) x) 1 2 3) -> 1)
  ( ((lambda (x . y) y) 1 2 3) -> (2 3))
  ( ((lambda (x . y) x) 1) -> 1)
  ( ((lambda (x . y) y) 1) -> ())
  ( ((lambda (x y . z) x) 1 2 3 4) -> 1 )
  ( ((lambda (x y . z) y) 1 2 3 4) -> 2 )
  ( ((lambda (x y . z) z) 1 2 3 4) -> (3 4))
  ( ((lambda (x y . z) z) 1 ) is an error)
  ( ((λ x x) 1 2 3 4 5) -> (1 2 3 4 5) )
  ( ((lambda (my-list) (my-list 1 2 3)) (lambda l l)) -> (1 2 3)  )
  
  ( ((λ y (* (car y) (car (cdr y)) (car (cdr (cdr y))))) 1 2 3 4) -> 6 )
  ( ((λ x (* (car x) (car (cdr x)))) 3 4) -> 12 )
  ( ((λ (x . y) (* x (* (car y) (car (cdr y))))) 1 2 3 4) -> 6 )
  ( ((λ (x . y) (set! x 10)(set! y '(20 30 40))(* x (* (car y) (car (cdr y)) (car (cdr (cdr y)))))) 1 2) -> 240000 )
  )

(define-test-set primitive-procedure-tests
  ((null? '()) -> #t )
  ((nonexistentprocedure) is an error)
  (sequence predefined-boolean-variables
            ;(true -> #t) plt base namespace doesn't have these
            ;(false -> #f)
            )
  (sequence cons-car-cdr
            ( (define c (cons 'a 'b)) )
            ( c -> (a . b))
            ( (car c) -> a )
            ( (cdr c) -> b ))
  (subsection numeric-operators
              ( (* 7 8) -> 56 )
              ( (+ 7 8) -> 15 )
              ( (/ 7 8) -> 7/8 )
              ((/ 1 0)is an error )
              ( (- 7 8) -> -1 ))
  (subsection comparison-operators
              ( (< 7 8) -> #t )
              ( (> 7 8) -> #f )
              ( (= 7 8) -> #f )
              ( (eq? '(1 2) '(1 2)) ->  #f)
              ( (eq? 1 1) ->  #t )
              ( (equal? '(1 2) '(1 2)) -> #t))
  (subsection miscellaneous-operators-used-in-later-tests
              ( (assv 'a '((a . 1)(b . 2))) -> (a . 1))
              ( (cadr '(a b)) -> b )
              ( (zero? 0) -> #t )
              ( (abs -1) -> 1 ))
  (subsection overridable
              ((let (( * (lambda(x y) (* x y)))) (* (* 2 1) 3)) -> 6)))


(define-test-set cond-tests
  ;base case
  ( (cond) )
  ;normal case
  ( (cond (#t 'hi)) -> hi)
  ( (cond (#t 'hi 'hello)) -> hello)
  ( (cond (#f 'hi)) )
  ;else clause
  ( (cond (else 'hi)) -> hi)
  ( (cond (else 'hi 'hello)) -> hello)
  ;duff clause
  ( (cond 1) is an error )
  ( (cond #f) is an error)
  ;singleton clause
  ( (cond (1)) -> 1) 
  ( (cond (#f)))
  ( (cond (#t)) -> #t)
  ;arrow clause
  ( (cond (#f => (lambda (x) x))) )
  ( (cond (#t => (lambda (x) x))) -> #t)
  ( (cond ('a => (lambda (x) x))) -> a)
  ;catch fall-throughs
  ( (cond (#f 'hi)(else 'lo)) -> lo)
  ( (cond (#f) (else 'lo)) -> lo)
  ;else in the wrong place
  ( (cond (else 'lo) (#f)) is an error) 
  ;various combinations
  ( (cond (#f) (#t)) -> #t)
  ( (cond (#t) (#f)) -> #t)
  ( (cond (#f) (#f) (else 'yo)) -> yo)
  ( (cond (#f) 1) is an error)
  ( (cond (#f) (1)) -> 1)
  ( (cond [else 5 6 7]) -> 7)
  ( (cond [#f 3] [#t 4] [#f] [1] [else 5]) -> 4)  
  ( (cond (#t 'zero 'one) (#f 'two) (else 'other)) -> one)
  ( (cond (())) is an error  )
  ( (cond (#f 'a)(else 'c)(#f 'b)) is an error )
  ( (cond (#f 'a)(else 'c)(#f 'b)) is an error )
  ( (cond (#f 'a)(else 'c)(#f 'b)) is an error )
  ( (cond (#f 'a)(else 'c)(#f 'b)) is an error )
  )

(define-test-set extended-cond-tests
  ( (cond [(= 0 1) (/ 0 0)][(< 2 0)(/ 0 0)][5]) -> 5 )
  ( ((lambda (var) (cond ((begin (set! var (+ var 1)) var) => (lambda(x) x)))) 0) -> 1)
  (sequence build-up-program
            ( (define (cdar x) (cdr (car x))) )
            ( (define (caar x) (car (car x))) )
            ( (define (explode) (/ 0 0))  )
            ( (define (my-assq a alist) (if (null? alist) #f (if (eq? (caar alist) a) alist (my-assq a (cdr alist))))) )
            ( (define mylist '((a . 1)(b . 2)))  )
            ( (cond 
                ((my-assq 'c mylist) => cdar)
                ((my-assq 'b mylist) => cdar)
                (else (explode))) -> 2 )
            ( (cond 
                ((my-assq 'c mylist) => cdar)
                (else (explode))) is an error )))  
  
(define-test-set complex-evaluations 
  ( ((λ (x) (* x x)) 2) -> 4 )
  ( ((lambda (x) (* x x)) 2) -> 4 )
  ( ((lambda (x) (cond ((= x 1) 'zero 'one) ((= x 2) 'two) (else 'other))) 1) -> one )
  ( ((lambda (x) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) 2) -> two )
  ( ((lambda (x) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) 3) -> other )
  ( ((lambda (x) (cons (cdr x) (car x))) (cons 'a 'b)) -> ( b . a) )
  ( ((lambda (n) ((lambda (fact) (fact fact n)) (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))))) 10 ) -> ,(* 1 2 3 4 5 6 7 8 9 10) )
  (sequence assignments
            ( ((lambda(a) (set! a 3) a) 2) -> 3 )
            ( (define global 2) )
            ( ((lambda(a) (set! global 3) a) 2) -> 2 )))

(define-test-set and-or-tests
  ( (and) -> #t )
  ( (and #t) -> #t )
  ( (and 1) -> 1 )
  ( (and #f (/ 1 0)) -> #f)
  ( (and #t #t #t #f (/ 1 0)) -> #f)
  ( (or) -> #f )
  ( (or #t) -> #t )
  ( (or 1) -> 1 )
  ( (or #t (/ 1 0)) -> #t)
  ( (or #t #t #t #t #f (/ 1 0)) -> #t)
  ( (or #f #f #f #t (/ 1 0)) -> #t))



(define-test-set let-tests
  ( (let ([x 5]) x) -> 5)
  ( (let ([b 2][a 1]) (cons a b)) -> (1 . 2))
  ( (let ([a 1][b 2][c 3]) (cons a (cons b (cons c '())))) -> (1 2 3))
  ( (let ([x 5]) (let ([x 2][y x]) (cons y (cons x '())))) -> (5 2))
  ( (let ((x 2)) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) -> two )
  ( (let ((x 3)) (cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))) -> other )
  ( (let () 'a) -> a )
  ( (let ([me "Tarzan"][you "Jane"])(let ([me you][you me])(cons me you))) -> ("Jane" . "Tarzan"))
  (subsection failures
              ( (let ((a)(b 2)) 'doom) is an error)
              ( (let (())) is an error)
              ( (let ((a 1))) is an error)
              ( (let ((a 1 b 2))) is an error)
              ( (let (()) body) is an error))  
  )

(define-test-set let*-tests 
  ((let* ((a 1) (b 2) (c 3)) (cons a (cons b (cons c '())))) -> (1 2 3))
  ((let* ((a 1) (b a) (c (+ a b))) (cons a (cons b (cons c '())))) -> (1 1 2))
  ((let* ((a 1) (b 2) (c 3)) (set! a c) (cons a (cons b (cons c '())))) -> (3 2 3))
  ((let* ((a 1) (b 2) (c a)) (set! c b) (cons a (cons b (cons c '())))) -> (1 2 2))
  ((let* () 'a) -> a)
  ((let* () 'a 'b) -> b)
  ((let* ((name (cons "Burroughs" '())) (name (cons "Rice" name)) (name (cons "Edgar" name))) name) -> ("Edgar" "Rice" "Burroughs"))
  (subsection failures
              ((let* ((a 1) (b) (c (+ a b))) (cons a (cons b (cons c '())))) is an error)
              ((let* ((b a) (a 1)) (cons b a)) is an error)
              ((let* ((a 1) (b 1))) is an error)
              ((let* (()) 'a) is an error)
              ((let* ((a 1) (b a) (c (+ a b))) (cons a (cons b (cons c '())))) -> (1 1 2))
              ((let* ((a b) (b 1)) (cons a b)) is an error)))

(define-test-set named-let-tests
  ( (let fac ([n 10]) (if (= n 0) 1 (* n (fac (- n 1))))) -> ,(* 10 9 8 7 6 5 4 3 2 1))
  ( (let fib ((n 10)) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) -> 55 ))

(define-test-set unclassified-as-yet
  ( (cond (#t 'hi)) -> hi )
  ( (cond (#f 'hi)) ))

(define-test-set ouch-differences-between-the-evaluators
  ( (cond (else)) is an error )
  ( (cond (1 2) '()) is an error )
  ( (begin (define (weird) (define x x) x) (weird)) is an error  )
  ( (let ((me "Robert")(me "Bob")) me) is an error)
  (sequence plt-top-level-bug-redefining-a-built-in-needs-done-twice 
            ;This sure looks like a bug. M. Felleisen 30/9/2008
            ( (define (assq a alist) (if (null? alist) #f (if (eq? (car (car alist)) a) alist (assq a (cdr alist))))) )
            ( (assq 'b '((a . 1)(b . 2))) -> ((b . 2)))
            ( (define (assq a alist) (if (null? alist) #f (if (eq? (car (car alist)) a) alist (assq a (cdr alist))))) )
            ( (assq 'b '((a . 1)(b . 2))) -> ((b . 2)) ))
  )



(define-test-set silly-iteration-construct-tests
  (sequence oddfilter
            ( (define (even? x) (if (= x 0) true  (odd?  (- x 1))))  )
            ( (define (odd?  x) (if (= x 0) false (even? (- x 1))))  )
            ( (list/c (* x x) for x in '(1 2 3 4 5 6 7 8 9 10) if (odd? x)) -> (1 9 25 49 81) but error in plt )
            ( (list/c x for x in (list/c (* x x) for x in '(1 2 3 4 5 6 7 8 9 10)) if (odd? x)) -> (1 9 25 49 81) but error in plt))
  ( (list/c (list/c (* x y) for x in '(1 2 3 4)) for y in '(3 4 5)) -> ((3 6 9 12) (4 8 12 16) (5 10 15 20)) but error in plt)
  ( (list/c (* x y) for x in '(1 2 3 4) for y in '(3 4 5)) -> (3 4 5 6 8 10 9 12 15 12 16 20) but error in plt )
  ( (list/c (* x y) for x in '(1 2 3 4) for y in '(3 4 5) if (= x y)) -> (9 16) but error in plt )
  ( (list/c (list/c (/ x y) for x in '(1 2 3) if (< x y)) for y in '(1 2 3) ) -> (()(1/2)(1/3 2/3)) but error in plt)
  ( (list/c (* x x) for x in '(1 2 3) if ) is an error))

(let ((totalpassed 0)
      (totalfailed 0))
  (for ((testset (list 
                  axiom-tests 
                  primitive-procedure-tests
                  cond-tests
                  complex-evaluations 
                  unclassified-as-yet
                  and-or-tests extended-cond-tests let-tests let*-tests named-let-tests variable-argument-list-tests
                  silly-iteration-construct-tests
                  #;ouch-differences-between-the-evaluators)))
    (let-values (((passfail comments) (interpret (car testset) (perform-test-set testset))))
      (set! totalpassed (+ totalpassed (car passfail)))
      (set! totalfailed (+ totalfailed (cdr passfail)))
      (for-each (λ(x) (printf "~a~n" x)) comments)))
  (printf "~a passed ~a failed~n" totalpassed totalfailed))

;running an expression that detects evaluation order
(printf "meta-evaluator's evaluation order is ~a~n" (meval '((lambda (a) (cons (set! a 'right-to-left) (set! a 'left-to-right)) a) false) (setup-environment)))
(printf "analyser's evaluation order is ~a~n" ((analyse '((lambda (a) (cons (set! a 'right-to-left) (set! a 'left-to-right)) a) false)) (setup-environment)))
(printf "the order in the underlying scheme is ~a~n" ((lambda (a) (cons (set! a 'right-to-left) (set! a 'left-to-right)) a) false))
(printf "the order in the underlying scheme via eval is ~a~n" (eval '((lambda (a) (cons (set! a 'right-to-left) (set! a 'left-to-right)) a) #f) (make-base-namespace)))

;evaluating an expression that is sensitive to details of internal definition
;sequential should work if we're just evaluating definitions like any other statement
(define only-sequential '(let  () 
                           (define a 10) 
                           (set! a (+ a 1)) 
                           (define b (+ a 1)) 
                           (+ a b)))

(define sequential-but-naughty '(let  () 
                                  (define a 10) 
                                  (define b (+ a 1)) 
                                  (set! a (+ a 1)) 
                                  (+ a b)))


















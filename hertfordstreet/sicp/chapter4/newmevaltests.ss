#lang scheme

(require "bookevaluator.ss")
(require "merror.ss")
(require scheme/help) 

(define testcode '((lambda (n) ((lambda (fact) (fact fact n)) (lambda (ft k) (if (< k 2) k (* k (ft ft (- k 1))))))) 10) )

(define testenvironment (setup-environment))

(define-struct timeresults (result cputime realtime gctime) #:transparent)
(define-struct threeway (code plt meval analysis execution) #:transparent)

(define (threewayeval testcode pltenvironment mevalenvironment analyseenvironment)
  (let ([plt       (with-handlers((exn:fail? (λ(v)'error)))(call-with-values (λ()(time-apply (λ()(eval testcode pltenvironment)) '())) make-timeresults)) ]
        [meval     (with-handlers((exn:meval? (λ(v)'error)))(call-with-values (λ()(time-apply (λ()(meval testcode mevalenvironment)) '())) make-timeresults)) ]
        [analysis  (with-handlers((exn:meval? (λ(v)'error)))(call-with-values (λ()(time-apply (λ()(analyse testcode)) '())) make-timeresults))])
    (if (eq? analysis 'error)
        (make-threeway testcode plt meval analysis 'error)
        (let* ([executionprocedure (car (timeresults-result analysis))]
               [execution (with-handlers((exn:meval? (λ(v)'error)))(call-with-values (λ()(time-apply (λ()(executionprocedure analyseenvironment)) '())) make-timeresults))])
          (make-threeway testcode plt meval analysis execution)))))

(define (cleanexecute testcode)
  (threewayeval testcode (make-base-namespace) (setup-environment) (setup-environment)))

(define (equalinsomesense plt meval analyse)
  (or (and (equal? plt (list (void))) (and (equal? meval analyse) (equal? analyse '(#f))))
      (and (equal? plt meval)
           (equal? plt analyse))))

(define (print-threeway t)
  (define (cpu-gc t)(-(timeresults-cputime t)(timeresults-gctime t)))
  (let ([plterr (eq? (threeway-plt t) 'error)]
        [mevalerr (eq? (threeway-meval t) 'error)]
        [analyseerr (or (eq? (threeway-analysis t) 'error)(eq? (threeway-execution t) 'error))])
    
    (cond  [(and plterr mevalerr analyseerr) (format "~a all errors" (threeway-code t))]
           [(or plterr mevalerr analyseerr) (format "~a" t)]
           [else 
            (let ([plt-results (timeresults-result (threeway-plt t))]
                  [meval-results (timeresults-result (threeway-meval t))]
                  [analyse-results (timeresults-result (threeway-execution t))]
                  [plt-time (cpu-gc (threeway-plt t))]      
                  [meval-time (cpu-gc (threeway-meval t))]      
                  [analysis-time (cpu-gc (threeway-analysis t))]      
                  [execution-time (cpu-gc (threeway-execution t))]      
                  )
        
              (let ([allequal (equalinsomesense plt-results meval-results analyse-results)])
                (if allequal
                    (format "~a all results equal: ~a plt ~a meval ~a analyse ~a=~a+~a" (threeway-code t) plt-results plt-time meval-time (+ analysis-time execution-time) analysis-time execution-time)
                    (format "~a" t))))])))

(print-threeway (cleanexecute testcode))


;pulled from r5rstest.scm
(define testlist 
  '(
    (test (cond) 'error)
    (test (cond ('a)) 'a)
    (test (cond (#f 'a) ('b)) 'b)
    (test (cond (#t 'a) (#t 'b)) 'a)
    (test (cond ((> 3 2) 'greater) ((< 3 2) 'less)) 'greater)
    (test (cond ((> 3 3) 'greater) ((< 3 3) 'less)  (else 'equal)) 'equal)
    (test (cond ((assv 'b '((a 1) (b 2))) => cadr)  (else #f)) 2)
    (test (cond (#f 2) (else 5)) 5)
    (test (cond (1 2) (else 5)) 2)
    (test (cond (1 => (lambda (x) (+ x 2))) (else 8)) 3)
    (test (cond ((+ 1 2))) 3)
    (test (cond ((zero? 1) 123) ((= 1 1) 321)) 321)
    (test (cond ('() 1)) 1)
    (test (cond (1 2) '()) 2)
    (test (cond (1 2 3)) 3)
    (test (cond ((= 1 2) 3) ((+ 3 4))) 7)
    (test (cond ((= 1 1) (abs -1) (+ 2 3) (* 10 2)) (else 123)) 20)
    (test (let ((a 1)) (cond ((= a 1) (set! a 2) (+ a 3)))) 5)
    (test (let ((a 1)) (cond ((= a 2) (+ a 2)) (else (set! a 3) (+ a 3)))) 6)
    
    (test (cond ((= 1 2) 3) (else 4) (4 5)) 'error)
    (test (cond ((+ 1 2) => (lambda (a b) (+ a b)))) 'error)
    (test (cond (else)) 'error)
    (test (cond (#t => 'ok)) 'error)
    (test (cond (else =>)) 'error)
    
    (test (cond (else 1)) 1)
    ;(test (call/cc (lambda (r) (cond ((r 4) 3) (else 1)))) 4)
    (test (cond ((cond (#t 1)))) 1)
    (test (cond (cond (#t 1))) 'error)
    (test (cond 1) 'error)))


(for ((i (map cadr testlist))) (printf "~a~n" (print-threeway (cleanexecute i))))
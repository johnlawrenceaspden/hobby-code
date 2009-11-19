#lang scheme

(require "merror.ss")

(define-syntax-rule (assert-error body)
  (with-handlers ((exn:meval? (λ(x) #t)))
    body
    #f))

(define (environment-tests extend-environment the-empty-environment lookup-variable-value set-variable-value! define-variable! environment->list-of-bindings)
  (define test-environment   (extend-environment '(b c) '(1 2) (extend-environment '(a) '(1) the-empty-environment)))
  (define larger-environment (extend-environment '(a b . c) '(1 2 3 4) test-environment) )
  (define varargs-environment (extend-environment '(p . q) '(1 2 3 4) (extend-environment 'x '(1 2 3 4) (extend-environment 'y '() test-environment))))
  (let ((tests (list 

                (equal? (environment->list-of-bindings varargs-environment) '((p q) (x) (y) (b c) (a)))
                (equal? (environment->list-of-bindings larger-environment) '((a b c) (b c) (a)))
                (equal? (environment->list-of-bindings test-environment) '((b c) (a)))

                (equal? (lookup-variable-value 'a test-environment) 1)
                (equal? (lookup-variable-value 'b test-environment) 1)
                (equal? (lookup-variable-value 'c test-environment) 2)
                (assert-error (lookup-variable-value 'd test-environment))
                (assert-error (set-variable-value! 'd 100 test-environment))
                (begin (define-variable! 'd 100 test-environment) #t)
                (begin (define-variable! 'c 'newc test-environment) #t)
                (begin (set-variable-value! 'd 200 test-environment) #t)
                (begin (set-variable-value! 'a 300 test-environment) #t)
                
                (equal? (lookup-variable-value 'b test-environment) 1)
                (equal? (lookup-variable-value 'c test-environment) 'newc)
                (equal? (lookup-variable-value 'd test-environment) 200)
                (equal? (lookup-variable-value 'a test-environment) 300)

                (equal? (lookup-variable-value 'b larger-environment) 2)
                (equal? (lookup-variable-value 'c larger-environment) '(3 4))
                (equal? (lookup-variable-value 'd larger-environment) 200)
                (equal? (lookup-variable-value 'a larger-environment) 1)
                
                (equal? (lookup-variable-value 'a varargs-environment) 300)
                (equal? (lookup-variable-value 'b varargs-environment) 1)
                (equal? (lookup-variable-value 'c varargs-environment) 'newc)
                (equal? (lookup-variable-value 'x varargs-environment) '(1 2 3 4))
                (equal? (lookup-variable-value 'y varargs-environment) '())
                (equal? (lookup-variable-value 'p varargs-environment) 1)
                (equal? (lookup-variable-value 'q varargs-environment) '(2 3 4))             
                
                (equal? (environment->list-of-bindings varargs-environment) '((p q) (x) (y) (d b c) (a)))
                (equal? (environment->list-of-bindings larger-environment) '((a b c) (d b c) (a)))
                (equal? (environment->list-of-bindings test-environment) '((d b c) (a)))
                
                (begin (define-variable! 'r 'rval varargs-environment) #t)
                (begin (define-variable! 'r 'rval larger-environment) #t)
                (begin (define-variable! 'r 'rval test-environment) #t)
                
                (equal? (environment->list-of-bindings varargs-environment) '((r p q) (x) (y) (r d b c) (a)))
                (equal? (environment->list-of-bindings larger-environment) '((r a b c) (r d b c) (a)))
                (equal? (environment->list-of-bindings test-environment) '((r d b c) (a)))
                )))
                (list (if (andmap (λ(x)x) tests) (format "all ~a tests passed" (length tests)) "TESTS FAILED") tests)))

(define (test environmentfilename)
  (list (format "testing the procedures provided by: ~a" environmentfilename)
        (environment-tests (dynamic-require environmentfilename 'extend-environment ) 
                           (dynamic-require environmentfilename 'the-empty-environment ) 
                           (dynamic-require environmentfilename 'lookup-variable-value ) 
                           (dynamic-require environmentfilename 'set-variable-value! ) 
                           (dynamic-require environmentfilename 'define-variable! )
                           (dynamic-require environmentfilename 'environment->list-of-bindings))))

(provide environment-tests)

(define (test-all)
  (list 
;   (test "environments.ss")
;   (test "environmentsaslists.ss")
   (test "environmentsasstructs.ss")))

(test-all)
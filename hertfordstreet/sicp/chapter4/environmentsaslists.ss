#lang scheme

(require scheme/enter)
(require (lib "trace.ss"))

(require "merror.ss")
  
(provide extend-environment
         the-empty-environment
         define-variable!
         lookup-variable-value
         set-variable-value!
         traceenvironmentoperations)

;an environment is simply a list of frames, built up
;from the empty environment by adding frames
(define the-empty-environment '())
(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

;frames are mutable tagged lists of mutable pairs of variables and values.
(define (make-empty-frame) (mcons 'frame '()))
(define (frame-bindings frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (let ((bindings (frame-bindings frame))
        (new-binding (mcons var val)))
    (set-mcdr! frame (mcons new-binding bindings))))

(define (make-frame variables values)
  (cond ((null? variables)
         (cond ((null? values) (make-empty-frame))
               (else (merror "not enough variables: variables ~a values: ~a" variables values ))))
        ((pair? variables)
         (cond ((pair? values)(let ((frame-so-far (make-frame (cdr variables)(cdr values))))
                                (add-binding-to-frame! (car variables) (car values) frame-so-far)
                                frame-so-far))
               (else (merror "not enough values: variables ~a values: ~a" variables values ))))
        (else 
         (cond ((or (null? values) (pair? values)) (let ((frame-so-far (make-empty-frame)))
                                                     (add-binding-to-frame! variables values frame-so-far)
                                                     frame-so-far))
               (else (error (format "weird error: variables ~a values: ~a" variables values) ))))))


(define (scan-frame frame var)
  (let loop ((bindings (frame-bindings frame)))
    (cond ((null? bindings) #f) 
          ((eq? var (mcar (mcar bindings)))  (mcar bindings))
          (else (loop (mcdr bindings))))))

(define (lookup-variable-value var env)
    (cond ((eq? env the-empty-environment) (merror "Unbound variable: ~a" var))
          ((scan-frame (first-frame env) var) => (λ(x) (let ((val(mcdr x))) (if (eq? val '*unassigned*) (merror "unassigned variable: ~a " var) val))))
          (else (lookup-variable-value var (enclosing-environment env)))))

(define (set-variable-value! var val env)
    (cond ((eq? env the-empty-environment) (merror "Unbound variable  -- SET! var: ~a val: ~a" var val))
          ((scan-frame (first-frame env) var) => (λ(x) (set-mcdr! x val)))
          (else (set-variable-value! var val (enclosing-environment env)))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (cond ((scan-frame frame var) => (λ(x) (set-mcdr! x val)))
          (else (add-binding-to-frame! var val frame)))))


(define (test)
  (printf "self-testing using the tests from environmenttests.ss")
  ((dynamic-require "environmenttests.ss" 'environment-tests) extend-environment the-empty-environment lookup-variable-value set-variable-value! define-variable!))

;(test)
(define (traceenvironmentoperations on)
  (if on (trace         extend-environment
                         define-variable!
                         lookup-variable-value
                         set-variable-value!)
          (untrace       extend-environment
                         define-variable!
                         lookup-variable-value
                         set-variable-value!)))
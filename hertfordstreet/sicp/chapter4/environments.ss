#lang scheme

(require "merror.ss")
(require scheme/enter)
  
(provide lookup-variable-value
         extend-environment
         define-variable!
         set-variable-value!
         the-empty-environment)


;an environment is simply a list of frames, built up
;from the empty environment by adding frames
(define the-empty-environment '())
(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

;frames are mutable pairs of mutable lists of variables and values.
(define (make-empty-frame) (mcons '() '()))
(define (frame-values frame) (mcdr frame))
(define (frame-variables frame) (mcar frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

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
         (cond ((or (null? values) 
                    (pair? values)) 
                (let ((frame-so-far (make-empty-frame)))
                  (add-binding-to-frame! variables values frame-so-far)
                  frame-so-far))
               (else (error (format "weird error: variables ~a values: ~a" variables values) ))))))

(define (scan-frame frame var)
  (let loop ((vars (frame-variables frame))
             (vals (frame-values    frame)))
    (cond ((null? vars) #f) 
          ((eq? var (mcar vars)) (cons vars vals))
          (else (loop (mcdr vars) (mcdr vals))))))

(define (lookup-variable-value var env)
    (cond ((eq? env the-empty-environment) (merror "Unbound variable: ~a" var))
          ((scan-frame (first-frame env) var) => (λ(x) (mcar (cdr x))))
          (else (lookup-variable-value var (enclosing-environment env)))))

(define (set-variable-value! var val env)
    (cond ((eq? env the-empty-environment) (merror "Unbound variable  -- SET! var: ~a val: ~a" var val))
          ((scan-frame (first-frame env) var) => (λ(x) (set-mcar! (cdr x) val)))
          (else (set-variable-value! var val (enclosing-environment env)))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (cond ((scan-frame frame var) => (λ(x) (set-mcar! (cdr x) val)))
          (else (add-binding-to-frame! var val frame)))))

(define (test)
  (printf "self-testing using the tests from environmenttests.ss")
  ((dynamic-require "environmenttests.ss" 'environment-tests) extend-environment the-empty-environment lookup-variable-value set-variable-value! define-variable!))

;(test)
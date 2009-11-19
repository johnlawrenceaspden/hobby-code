#lang scheme

;debugging aids
(require (lib "trace.ss"))
(require scheme/help)
(require scheme/enter)

;environment procedures defined separately
;(require "environments.ss")
(require "environmentsaslists.ss")
(require "merror.ss")

(provide (all-defined-out))

;                                                           
;                                                           
;                      ;    ;                      ;        
;                      ;    ;                      ;        
;                      ;   ;                       ;        
;    ;;;  ;   ;  ;;;   ;   ;   ;;;   ; ;;   ; ;;   ;  ;   ; 
;   ;   ; ;   ; ;   ;  ;   ;  ;   ;  ;;  ;  ;;  ;  ;  ;   ; 
;   ;   ;  ; ;      ;  ;  ;       ;  ;   ;  ;   ;  ;   ; ;  
;   ;;;;;  ; ;    ;;;  ;  ;     ;;;  ;   ;  ;   ;  ;   ; ;  
;   ;      ;;;  ;;  ;  ;  ;   ;;  ;  ;   ;  ;   ;  ;   ;;;  
;   ;   ;   ;   ;  ;;  ; ;    ;  ;;  ;;  ;  ;;  ;  ;    ;   
;    ;;;    ;    ;; ;  ; ;     ;; ;  ; ;;   ; ;;   ;    ;   
;                                    ;      ;           ;   
;                                    ;      ;          ;;   
;                                    ;      ;               

(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (meval (and->if exp) env))
        ((or?  exp) (meval (or->if exp) env))
        ((lambda? exp)
         (make-compound-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp)
         (meval (let->lambda exp) env))
        ((let*? exp)
         (meval (let*->lets exp) env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (meval (cond->if exp) env))
        ((application? exp)
         (mapply (meval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
         (merror "Unknown expression type -- EVAL exp: ~a env: ~a" exp env))))

(define (mapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (compound-procedure-body procedure)
          (extend-environment (compound-procedure-parameters procedure) arguments (compound-procedure-environment procedure))))
        (else
         (merror "Unknown procedure type -- APPLY procedure: ~a" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let* ((b (list-of-values (rest-operands exps) env))
             (a (meval (first-operand exps) env)))
        (cons a b)))) ;forcing right-to-left evaluation here

(define (eval-if exp env)
  (if (true? (meval (if-predicate exp) env))
      (meval (if-consequent exp) env)
      (meval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((empty-sequence? exps) #f)
        ((last-exp? exps) (meval (first-exp exps) env))
        (else (meval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (meval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (meval (definition-value exp) env)
    env)
  'ok)



;                                                                         
;                                                                         
;   ;                                                                     
;   ;                                                    ;                
;   ;                                                    ;                
;   ; ;;    ;;;    ;;;    ;;;        ;;;   ;   ;  ; ;;  ;;;   ;;;   ;   ; 
;   ;;  ;  ;   ;  ;   ;  ;   ;      ;   ;  ;   ;  ;;  ;  ;   ;   ;   ; ;  
;   ;   ;      ;  ;;     ;   ;      ;;      ; ;   ;   ;  ;       ;   ;;;  
;   ;   ;    ;;;   ;;;   ;;;;;       ;;;    ; ;   ;   ;  ;     ;;;    ;   
;   ;   ;  ;;  ;      ;  ;              ;   ;;;   ;   ;  ;   ;;  ;   ;;;  
;   ;;  ;  ;  ;;  ;   ;  ;   ;      ;   ;    ;    ;   ;  ;   ;  ;;   ; ;  
;   ; ;;    ;; ;   ;;;    ;;;        ;;;     ;    ;   ;  ;;   ;; ;  ;   ; 
;                                            ;                            
;                                           ;;                            
;                                          ;;                             


(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((boolean? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))


(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) 
  (if (not (null? (cddr exp)))
      (caddr exp)
      (merror "no value to assign in assignment expression ~a" exp)))


(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body


(define (lambda? exp) (or (tagged-list? exp 'lambda) (tagged-list? exp 'λ)))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (empty-sequence? seq) (null? seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (true? x)
  (not (eq? x #f)))
(define (false? x) ; this is never called!
  (eq? x #f))

;                                                                  
;                                                                  
;                                       ;                          
;                                       ;                          
;                                       ;                          
;   ; ;;   ; ;  ;;;    ;;;    ;;;    ;; ;  ;   ;  ; ;  ;;;    ;;;  
;   ;;  ;  ;   ;   ;  ;   ;  ;   ;  ;  ;;  ;   ;  ;   ;   ;  ;   ; 
;   ;   ;  ;   ;   ;  ;      ;   ;  ;   ;  ;   ;  ;   ;   ;  ;;    
;   ;   ;  ;   ;   ;  ;      ;;;;;  ;   ;  ;   ;  ;   ;;;;;   ;;;  
;   ;   ;  ;   ;   ;  ;      ;      ;   ;  ;   ;  ;   ;          ; 
;   ;;  ;  ;   ;   ;  ;   ;  ;   ;  ;  ;;  ;  ;;  ;   ;   ;  ;   ; 
;   ; ;;   ;    ;;;    ;;;    ;;;    ;; ;   ;; ;  ;    ;;;    ;;;  
;   ;                                                              
;   ;                                                              
;   ;                                                         ;    



;compound procedures (which contain their body text)
(define (make-compound-procedure parameters body env)
  (if (null? body) (merror "empty procedure body -- MAKE-COMPOUND-PROCEDURE")
      (list 'compound-procedure parameters body env)))
(define (compound-procedure? p)
  (tagged-list? p 'compound-procedure))
(define (compound-procedure-parameters p) (cadr p))
(define (compound-procedure-body p) (caddr p))
(define (compound-procedure-environment p) (cadddr p))


;                                                        
;                                                        
;              ;             ;      ;                    
;                               ;                        
;                               ;                        
;   ; ;;   ; ; ;  ; ;;  ;;   ; ;;;  ; ;   ;  ;;;    ;;;  
;   ;;  ;  ;   ;  ;;  ;   ;  ;  ;   ; ;   ; ;   ;  ;   ; 
;   ;   ;  ;   ;  ;   ;   ;  ;  ;   ;  ; ;  ;   ;  ;;    
;   ;   ;  ;   ;  ;   ;   ;  ;  ;   ;  ; ;  ;;;;;   ;;;  
;   ;   ;  ;   ;  ;   ;   ;  ;  ;   ;  ;;;  ;          ; 
;   ;;  ;  ;   ;  ;   ;   ;  ;  ;   ;   ;   ;   ;  ;   ; 
;   ; ;;   ;   ;  ;   ;   ;  ;  ;;  ;   ;    ;;;    ;;;  
;   ;                                                    
;   ;                                                    
;   ;                                            ;; ;;;; 


(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))


(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '* *)
        (list '+ +)
        (list '/ /)
        (list '- -)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'assv assv)
        (list 'cadr cadr)
        (list 'zero? zero?)
        (list 'abs abs)
        ;<more primitives>
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))


(define (apply-primitive-procedure proc args)
  (with-handlers ((exn:fail? (λ(v)(merror "exception thrown by primitive operation proc: ~a on args: ~a [~a]" proc args v))))
    (apply
     (primitive-implementation proc) args)))

;                                                                                                                    
;                                                                                                                    
;       ;             ;                  ;                                                    ;                      
;       ;                                ;                                                                           
;       ;                                ;                                                                           
;    ;; ;   ;;;   ; ; ; ;   ;  ;;;    ;; ;       ;;;   ;   ;  ; ;;   ; ;  ;;;    ;;;    ;;;   ;   ;;;   ; ;;    ;;;  
;   ;  ;;  ;   ;  ;   ; ;   ; ;   ;  ;  ;;      ;   ;   ; ;   ;;  ;  ;   ;   ;  ;   ;  ;   ;  ;  ;   ;  ;;  ;  ;   ; 
;   ;   ;  ;   ;  ;   ;  ; ;  ;   ;  ;   ;      ;   ;   ;;;   ;   ;  ;   ;   ;  ;;     ;;     ;  ;   ;  ;   ;  ;;    
;   ;   ;  ;;;;;  ;   ;  ; ;  ;;;;;  ;   ;      ;;;;;    ;    ;   ;  ;   ;;;;;   ;;;    ;;;   ;  ;   ;  ;   ;   ;;;  
;   ;   ;  ;      ;   ;  ;;;  ;      ;   ;      ;       ;;;   ;   ;  ;   ;          ;      ;  ;  ;   ;  ;   ;      ; 
;   ;  ;;  ;   ;  ;   ;   ;   ;   ;  ;  ;;      ;   ;   ; ;   ;;  ;  ;   ;   ;  ;   ;  ;   ;  ;  ;   ;  ;   ;  ;   ; 
;    ;; ;   ;;;   ;   ;   ;    ;;;    ;; ;       ;;;   ;   ;  ; ;;   ;    ;;;    ;;;    ;;;   ;   ;;;   ;   ;   ;;;  
;                                                             ;                                                      
;                                                             ;                                                      
;                                                             ;                                               ;   ; ;

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-clause? clause) (and (pair? clause) (>= (length clause) 1)))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-arrow-clause? clause)
  (and (= (length clause) 3) (eq? (cadr clause) '=>)))
(define (cond-arrow-function clause)
  (caddr clause))
(define (cond-single-clause? clause) (= (length clause) 1))

(define (cond->if exp)
  (let ([clauses (cond-clauses exp)])
    (if (null? clauses) #f
        (let ([firstclause (car clauses)]
              [restofclauses (cdr clauses)])
          (cond [(not (cond-clause? firstclause)) (merror "bad cond clause ~a in ~a" firstclause exp)]
                [(cond-else-clause? firstclause) (if (null? restofclauses)
                                                     `(begin ,@(cond-actions firstclause))
                                                     (merror "ELSE clause isn't last -- EVAL-COND ~a" exp))]
                [(cond-arrow-clause? firstclause)
                 (let ([test (gensym)])
                 `(let ([,test ,(cond-predicate firstclause)])
                    (if ,test (,(cond-arrow-function firstclause) ,test)
                        ,(cond->if `(cond ,@restofclauses)))))]
                [(cond-single-clause? firstclause)
                 (let ([test (gensym)])
                 `(let ([,test ,(cond-predicate firstclause)])
                    (if ,test ,test
                        ,(cond->if `(cond ,@restofclauses)))))]
                [else
                 `(if ,(cond-predicate firstclause) (begin ,@(cond-actions firstclause))
                      ,(cond->if `(cond ,@restofclauses)))]
                )))))


(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))

(define (and->if exp)
  (let ([clauses (and-clauses exp)])
    (cond  [(null? clauses) 'true]
           [(null? (cdr clauses)) (car clauses)]
           [else `(if ,(car clauses) ,(and->if (cons 'and (cdr clauses))) false)])))

(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (or->if exp)
  (let ([clauses (or-clauses exp)])
    (cond [(null? clauses) #f]
          [(null? (cdr clauses)) (car clauses)]
          [else `(if ,(car clauses) #t ,(or->if (cons 'or (cdr clauses))))])))


(define (let*? exp)  (tagged-list? exp 'let*))

(define (let*->lets expression)
  (define (let*-helper vars exps body)
    (cond [(null? vars) `(let (),@body)]
          [(null? (cdr vars)) `(let ((,(car vars) ,(car exps))) ,@body)]
          [else `(let ((,(car vars) ,(car exps))) ,(let*-helper (cdr vars) (cdr exps) body))]))
  (match expression
    ((list 'let* (list (list var exp) ...) body ...)
     (cond [(null? body) (merror "null body in: ~a" expression)]
           [else (let*-helper var exp body)]))
    (_ (merror "bad syntax in ~a" expression))))


(define (let? exp)  (tagged-list? exp 'let))

(define (let->lambda exp)
  (match exp
    ((list 'let name (list (list v e) ...) body1 body2 ...) 
     `(let ((,name '<undefined>))
        (set! ,name (lambda ( ,@v ) ,body1 ,@body2 ))
        (,name ,@e)))
    ((list 'let (list (list v e) ...) body1 body2 ...)
     `((lambda ( ,@v ) ,body1 ,@body2) ,@e))
    (else (merror "bad syntax in let: ~a" exp))))


;                       
;                       
;                     ; 
;                     ; 
;                     ; 
;   ; ;  ;;;   ; ;;   ; 
;   ;   ;   ;  ;;  ;  ; 
;   ;   ;   ;  ;   ;  ; 
;   ;   ;;;;;  ;   ;  ; 
;   ;   ;      ;   ;  ; 
;   ;   ;   ;  ;;  ;  ; 
;   ;    ;;;   ; ;;   ; 
;              ;        
;              ;        
;              ;   ;  ;;

;use meta-evaluator from ordinary scheme repl using me
(define the-global-environment (setup-environment))
(define-syntax-rule (me exp) (meval 'exp the-global-environment))

;(void (me (define (help) "no help to be had here")))


;use it in a REPL loop
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (not (eq? input eof))
        (let ((output 
               (with-handlers ((exn:meval? (λ(v)(format "~a" v))))(meval input the-global-environment))))
          (announce-output output-prompt)
          (user-print output)
          (driver-loop))
        (printf "bye!~n"))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (compound-procedure-parameters object)
                     (compound-procedure-body object)
                     '<procedure-env>))
      (display object)))


(printf 
"Welcome to the meta-evaluator from Chapter 4 of SICP....
to evaluate an expression in the top level environment
use me. eg (me ((lambda(x)(* x x)) 2)).
To run a meta-repl, use (driver-loop)
To see code coverage in drscheme, use Language/Choose Language/Module/Syntactic test suite coverage before running the program.
")












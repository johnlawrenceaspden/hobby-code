;;; pp-syntax.ss --  Jens Axel Soegaard  -- 27th july 2005
(module pp-syntax mzscheme
  (provide unexpand unexpand-to-datum pp-syntax)
  
  ;; PURPOSE
  ;;   This file contains functions that pretty prints the
  ;; output of EXPAND. The pretty printer attempts to present
  ;; the output of EXPAND using simple expression. E.g.
  ;; EXPAND will expand (let ((x 1)) 2) into (let-values (((x) 1)) 2),
  ;; which the pretty printer will "unexpand" into (let ((x 1)) 2).
  
  ;; FUNCTIONS
  
  ; unexpand : fully-expanded-syntax -> syntax
  ;      Unexpand a piece of fully expanded syntax.
  ;      E.g. simple occurences of let-values are rewritten to use let.
  ; unexpand-to-datum : fully-expanded-syntax -> datum
  ;      Unexpand a piece of fully expanded syntax, and return
  ;      the result as a datum.
  ; pp-syntax : fully-expanded-syntax -> 
  ;      Pretty-prints the unexpanded piece of syntax
  
  ;; EXAMPLE
  
  ;> (pp-syntax (expand
  ;              '(begin
  ;                 (letrec ((f (lambda (n)
  ;                               (if (= n 0)
  ;                                   1
  ;                                   (* n (f (- n 1)))))))
  ;                   (begin0 (f 5)
  ;                           (set! x 1)))
  ;                 (define y (let ((z 3)) 2)))))
  
  ;;  [prints]
  
  ;  (begin
  ;    (letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))))
  ;      (begin0 (f 5) (set! x 1)))
  ;    (define y (let ((z 3)) 2)))
  
  ;; For comparison  
  ; (pretty-print (syntax-object->datum (expand <same expression>)))
  ; prints
  
  ;  (begin
  ;  (letrec-values (((f)
  ;                   (lambda (n)
  ;                     (if (#%app (#%top . =) n (#%datum . 0))
  ;                       (#%datum . 1)
  ;                       (#%app
  ;                        (#%top . *)
  ;                        n
  ;                        (#%app
  ;                         f
  ;                         (#%app (#%top . -) n (#%datum . 1))))))))
  ;    (begin0 (#%app f (#%datum . 5)) (set! x (#%datum . 1))))
  ;  (define-values
  ;    (y)
  ;    (let-values (((z) (#%datum . 3))) (#%datum . 2))))
  
  
  (require (lib "pretty.ss"))
  (pretty-print-columns 70)  
  
  (define (self-evaluating? o)
    (or (boolean? o)
        (number? o)
        (string? o)
        (char? o)))
  
  (define (smap f . sl)
    (define (->list o)
      (if (syntax? o) (syntax->list o) o))
    (apply map f (map ->list sl)))
  
  
  ;expr is one of
  ;  variable
  ;  (lambda formals expr ···1)
  ;  (case-lambda (formals expr ···1) ···)
  ;  (if expr expr)
  ;  (if expr expr expr)
  ;  (begin expr ···1)
  ;  (begin0 expr expr ···)
  ;  (let-values (((variable ···) expr) ···) expr ···1)
  ;  (letrec-values (((variable ···) expr) ···) expr ···1)
  ;  (set! variable expr)
  ;  (quote datum)
  ;  (quote-syntax datum)
  ;  (with-continuation-mark expr expr expr)
  ;  (#%app expr ···1)
  ;  (#%datum . datum)
  ;  (#%top . variable)
  ;  (#%variable-reference variable)
  ;  (#%variable-reference (#%top . variable))
  
  ; pp-expr : syntax-object -> syntax-object
  (define (pp-expr so)
    (define pe pp-expr)
    (define (pe* sl) (smap pe sl))
    (syntax-case so (lambda if begin begin0 let-values letrec-values set! 
                      with-continuation-mark #%datum #%app #%top #%variable-reference 
                      and or)
      ; AND
      [(if e1 e2 (#%datum . #f))
       (pe #`(and #,(pe #'e1) #,(pe #'e2)))]
      [(and e1 ... (and e2 ...)) 
       (pe #'(and e1 ... e2 ...))]
      [(and (and e1 ...) e2 ...) 
       (pe #'(and e1 ... e2 ...))]
      [(and expr ...)
       #`(and #,@(pe* #'(expr ...)))]
      ; OR
      [(let-values (((or-part1) x)) (if or-part2 or-part3 y))
       (and (and (identifier? #'or-part2)
                 (identifier? #'or-part3))
            (eq? (syntax-e #'or-part1) (syntax-e #'or-part2))
            (eq? (syntax-e #'or-part2) (syntax-e #'or-part3)))
       (pe #`(or #,(pe #'x) #,(pe #'y)))]
      [(or expr1 ... (or expr2 ...))
       (pe #'(or expr1 ... expr2 ...))]
      [(or (or expr1 ...) expr2 ...)
       (pe #'(or expr1 ... expr2 ...))]
      [(or expr ...)
       #`(or #,@(pe* #'(expr ...)))]
      ; OTHER
      [(lambda formals expr ...)
       #`(lambda formals #,@(pe* #'(expr ...)))]
      [(if expr1 expr2)
       #`(if #,(pe #'expr1) #,(pe #'expr2))]
      [(if expr1 expr2 expr3)
       #`(if #,(pe #'expr1) #,(pe #'expr2) #,(pe #'expr3))]
      [(begin expr ...)
       #`(begin #,@(pe* #'(expr ...)))]
      [(begin0 expr ...)
       #`(begin0 #,@(pe* #'(expr ...)))]
      [(let-values (((id) expr) ...) body ...)
       #`(let #,(smap list #'(id ...) (pe* #'(expr ...)))
           #,@(pe* #'(body ...)))]
      [(letrec-values (((id) expr) ...) body ...)
       #`(letrec #,(smap list #'(id ...) (pe* #'(expr ...)))
           #,@(pe* #'(body ...)))]
      [(set! var expr)
       #`(set! var #,(pe #'expr))]
      [(with-continuation-mark expr1 expr2 expr3)
          #`(with-continuation-mark #,(pe #'expr1) #,(pe #'expr2) #,(pe #'expr3))]
      [(#%datum . o)
       (self-evaluating? (syntax-object->datum #'o))
       #'o]
      [(#%app expr ...)
       (smap pe #'(expr ...))]
      [(#%top . id)
       #'id]
      ; #%variable-reference is left untouched
      [_
       so]))
  
  ;general-top-level-expr is one of
  ;  expr
  ;  (define-values (variable ···) expr)
  ;  (define-syntaxes (identifier ···) expr)
  ;  (define-values-for-syntax (variable ···) expr)
  ;  (require require-spec ···)
  ;  (require-for-syntax require-spec ···)
  ;  (require-for-template require-spec ···)
  
  ; pp-general-top-level-expr : syntax-object -> syntax-object
  (define (pp-general-top-level-expr so)
    (syntax-case so (define-values define-syntaxes define-values-for-syntax 
                      require require-for-syntax require-for-template)
      [(define-values (var) expr)
       #`(define var #,(pp-expr #'expr))]
      [(define-values (var ...) expr)
       #`(define-values (var ...) #,(pp-expr #'expr))]
      [(define-syntaxes id expr)
       #`(define-syntax id #,(pp-expr #'expr))]
      [(define-syntaxes (id ...) expr)
       #`(define-syntaxes (id ...) #,(pp-expr #'expr))]
      [(define-values-for-syntax (var ...) expr)
       #`(define-values-for-syntax (var ...) #,(pp-expr #'expr))]
      [(require require-spec ...)
       #'(require require-spec ...)]
      [(require-for-syntax require-spec ...)
       #'(require-for-syntax require-spec ...)]
      [(require-for-template require-spec ...)
       #'(require-for-template require-spec ...)]
      [_
       (pp-expr so)]))
  
  ;top-level-expr is one of
  ;  general-top-level-expr
  ;  (module identifier name (#%plain-module-begin module-level-expr ···))
  ;  (begin top-level-expr ···)
  
  ; pp-top-level-expr : syntax-object -> syntax-object
  (define (pp-top-level-expr so)
    (syntax-case so (module begin #%plain-module-begin)
      [(module id name (#%plain-module-begin module-level-expr ...))
       #`(module id name (#%plain-module-begin #,@(smap pp-module-level-expr #'(module-level-expr ...))))]
      [(begin top-level-expr ...)
       #`(begin #,@(smap pp-top-level-expr #'(top-level-expr ...)))]
      [_
       (pp-general-top-level-expr so)]))
  
  ;module-level-expr is one of
  ;  general-top-level-expr
  ;  (provide provide-spec ...)
  ;  (begin module-level-expr ···)
  
  ; pp-module-level-expr : syntax-object -> syntax-object
  (define (pp-module-level-expr so)
    (syntax-case so (provide begin)
      [(provide provide-spec ...)
       #'(provide provide-spec ...)]
      [(begin module-level-expr ...)
       #`(begin #,@(smap pp-module-level-expr #'(module-level-expr ...)))]
      [_
       (pp-general-top-level-expr so)]))
  
  ; unexpand : fully-expanded-syntax -> syntax
  (define (unexpand so)
    (pp-module-level-expr
     (expand so)))
  
  ; unexpand-to-datum : fully-expanded-syntax -> datum
  (define (unexpand-to-datum so)
    (syntax-object->datum
     #`#,(unexpand so)))
  
  ; pp-syntax : fully-expanded-syntax -> 
  (define (pp-syntax so)
    (pretty-display
     (unexpand-to-datum so)))
  )
#lang scheme

;macro tutorial
;www.cs.utah.edu/classes/cs6520-mflatt/s04/macro-tutorial.pdf

(define-syntax swap
  (syntax-rules ()
    ((swap a b) (let ((tmp b))
                  (set! b a)
                  (set! a tmp)))))

;lexical scope!
(let ((tmp 5)
      (other 6))
  (swap tmp other)
  (list tmp other))

;lexical scope means that we can use local macros too
(define (f x)
  (define-syntax swap-with-arg
    (syntax-rules ()
      ((swap-with-arg y) (swap x y))))
  (list x (let ((z 12)
                (x 10))
            (swap-with-arg z)
            (list x z))))

(f 5)

;ok, how about rotate?

(define-syntax rotate
  (syntax-rules ()
    ((rotate a) (void))
    ((rotate a b c ...) (begin
                          (swap a b)
                          (rotate b c ...)))))

(let ((a 1)(b 2)(c 3))
  (rotate a b c)
  (list a b c))

;works at repl but not in code, don't know why
;(syntax->datum (expand-once '(rotate a b c)))


;that rotate is a bit mangy. We can do better
(define-syntax better-rotate
  (syntax-rules ()
    ((better-rotate a c ...)
     (shift-to (c ... a) (a c ...)))))

;to define the sub-macro
;we take advantage of the fact that the pattern matching is too good to be true.
(define-syntax shift-to
  (syntax-rules ()
    ((shift-to (from0 from ...) (to0 to ...))
     (let ((tmp from0))
       (set! to from) ...
       (set! to0 tmp)))))

(let ((a 1)(b 2)(c 3))
  (better-rotate a b c)
  (list a b c))

;(syntax->datum (expand-once '(better-rotate a b c)))

;identifier macros (utterly contrived and useless example?)

(define clocktime "17:00")
(define (get-clock) clocktime)
(define (put-clock! a) (set! clocktime a))


(define-syntax clock
  (syntax-id-rules (set!)
    ((set! clock e) (put-clock! e))
    ((clock a ...) (print "na"))
    (clock (get-clock))))



;;got bored here as he goes on to do call-by-reference using syntax-rules

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;http://c2.com/cgi-bin/wiki?DefineSyntax (fixed up by me because it was broken)

(define-syntax aif
  (lambda (expr)
    (syntax-case expr ()
      ((aif test then)
       (with-syntax ((it-id (datum->syntax (syntax aif) 'it)))
         (syntax
          (let ((tmp test))
            (if tmp
                (let ((it-id tmp)) then))))))
      ((aif test then else)
       (with-syntax ((it-id (datum->syntax (syntax aif) 'it)))
         (syntax
          (let ((tmp test))
            (if tmp
                (let ((it-id tmp)) then)
                (let ((it-id tmp)) else)
                ))))))))

(let ((x 1)) (aif (> x 2) (cons it it) (cons it '())))
(let ((x 3)) (aif (> x 2) (cons it it) (cons it '())))

; a cut down anaphoric when

(define-syntax awhen
  (lambda (expr)
    (syntax-case expr ()
      ((awhen test then)
       (with-syntax ((it-id (datum->syntax (syntax awhen) 'it)))
         (syntax
          (let ((tmp test))
            (if tmp
                (let ((it-id tmp)) then)
                (void)))))))))


(awhen (assq 1 '((1 . hello) (2 . bye))) (print (cdr it)))
;of course you can sort of do that with cond anyway
(cond ((assq 1 '((1 . hello) (2 . bye))) => (λ(x)(print (cdr x)))))



;my first syntax-case macro
(define-syntax fluffy
  (lambda (expr)
    (syntax-case expr ()
      ((fluffy a b)
       (syntax (printf "~a ~a~n" a b))))))

;behold its glory
(fluffy 'hello 'world)

;http://www.scheme.com/tspl2d/syntax.html

;Hang on, I see how it must work. Syntax objects are just list structure with lexical data attached.
;they have their own quote unquote quasiquote operators, called syntax unsyntax quasisyntax, whose 
;abbreviations are #' #, #` 
(define syntaxobjectlist (list
                          (syntax a)
                          #'a
                          #'(a b c)
                          (let ((x 'a)) #'(a b c x))
                          (let ((x 'a)) #'(a b c ,x))
                          (let ((x 'a)) #'(a b c #,x))
                          (let ((x 'a)) #`(a b c #,x))))

; syntax->datum strips the syntactic information, just leaving the code without its context.
(map syntax->datum syntaxobjectlist)


;I'm conjecturing here, but I think syntax-case is just a function which does pattern matching
;usually it's used to return syntax objects
(syntax-case '(random expression) () (_ (syntax (b a))))
;but it doesn't have to:
(syntax-case '(random expression with literal) (literal) (_ 2))
;no, backtracking here. It must be a macro to do this substitution
(syntax-case '(random expression with literal) (literal) ((random a b literal) (syntax (a b))))
;it appears that it can substitute pattern matching variables into syntax expressions
(syntax-case '(random expression with literal) (literal) ((random a b literal) (syntax (a b))))
;the pattern matching is pretty powerful
(syntax->datum (syntax-case '(cleantests evaltests (test1 result1) (test2 result2) (test3  result3)) () ((_ name (a b) ...) (syntax (testsuite name (cleantest a b) ...)))))
;enabling me to write test macros beyond my wildest dreams
(syntax->datum (syntax-case '(cleantests evaltests (1 -> 1) (((λ(x)2) 1) -> 2)) (->) ((_ name (t -> r) ...) (syntax (testsuite name (cleantest t r) ...)))))

;here's that wrapped up into a usable macro (relies on testsuite and cleantest already being defined though)
(define-syntax paralleltestsincleanenvironment
  (lambda (stx)
    (syntax-case stx (->)
      ((_ name (a -> b) ...) (syntax (testsuite name (cleantest a b) ...))))))

;of course it's slightly vexing to realize that after all that I could have done it with define-syntax-rule
(define-syntax-rule (ptce1 name (a -> b) ...) (testsuite name (cleantest a b) ...))

;although actually that doesn't work because -> is a variable, so it matches (a b c) as well as (a -> b)
(define-syntax ptce
  (syntax-rules (->)
    (_ (a -> b) ...) (testsuite name (cleantest a b) ...)))







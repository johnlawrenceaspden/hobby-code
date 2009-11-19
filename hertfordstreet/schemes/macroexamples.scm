(require (planet "pp-syntax.ss" ("soegaard" "syntax.plt")))

;Examples of how to write macros

;First a little infrastructure. The examples are lower down.

;single step macro expansion
(define (check-macro input)
  (syntax-object->datum (expand-once input)))

;several step macro expansion
(define (check-macro-n n originalmacro)
  (let loop ((i n) (macro originalmacro))
    (if (= i 0) macro
        (begin
          (loop (- i 1) (check-macro macro))))))

(define (iterate-until-stable guess lastguess improve)
  (if (equal?  guess lastguess) 
      guess
      (begin
        (display guess)
        (display "\n")
        (iterate-until-stable (improve guess) guess improve))))

;keep expanding the macro until it stops changing
(define (watch-macro-expansion macro)
  (iterate-until-stable macro '() check-macro))

; finally some macros!

; debugging macro
;(dbpr a)
;print e.g "a = 2" and pass the value on
;
;(dbpr a b c)
;print a = 2, b = 3, c = 4 and return c

(define-syntax dbpr
  (syntax-rules ()
    ((dbpr a) 
     (let ((temp a))
       (display 'a) (display  " = ") (display temp) (display "\n") 
       temp))
    ((dbpr a b ...)
     (let ((temp a)) (display 'a) (display " = ") (display temp) (display ", ") (dbpr b ...))
     )))

;lisp style macro to do debug-printing (of one argument only)
(define-macro (lispdp x)
  (let ((temp (gensym)))
    `(begin 
       (let ((,temp ,x)) 
         (display (format  "~a = ~a\n" (quote ,x) ,temp))
         ,temp))))

;here we watch a do loop in action
(define exampleloop 
  '(do ((i 1 (+ i 1))
        (j 1 (* j 2))
        (k 1 (* j k)))
     ((= i 5))
     (dbpr i j k) 
     (display (format "(loop) i,j,k = ~a,~a,~a\n" (dbpr i) j k))
     ))

;(eval exampleloop)
;(check-macro exampleloop)
;(check-macro (check-macro exampleloop))

;functional for loop
;
;(for (10) (stuff) (otherstuff))
;
;(for ( i 0 9 ) (stuff i) (otherstuff i))
;
;(let loop ((i 1))
;  (when (<= i 10)
;    (begin
;      (stuff i)
;      (otherstuff i))
;    (loop (+ i 1))))

(define-syntax for
  (syntax-rules ()
    ((for (times) c ...)  (for (i 1 times) c ...)) ; for (10) -> for (i 1 10)
    ((for (i start end) c ...)
     (let loop ((i start))
       (when (<= i end)
         (begin
           c ...)
         (loop (+ i 1)))))))


; python-style generators (due to Will Farr)
; http://wmfarr.blogspot.com/2006/08/one-more-example-of-python-generators.html
(define-syntax define-generator
  (lambda (stx)
    (syntax-case stx ()
      ((define-generator (name arg ...) body0 body1 ...)
       (with-syntax ((yield (datum->syntax-object 
                             (syntax body0) 
                             'yield)))
         (syntax
          (define (name arg ...)
            (letrec ((continue-k #f)
                     (return-k #f)
                     (yield
                      (lambda args
                        (let/cc cont
                          (set! continue-k cont)
                          (call-with-values 
                           (lambda () (apply values args)) 
                           return-k)))))
              (lambda ()
                (let/cc ret
                  (set! return-k ret)
                  (if continue-k
                      (continue-k '())
                      (begin
                        body0 body1 ...
                        (error 'name "reached end of generator values")))))))))))))

;example generator code 

;(define-generator (nums-from n)
;  (let loop ((i n))
;    (yield i)
;    (loop (+ i 1))))
;
;(define ten-on (nums-from 10))
;
;(for (10) (display (ten-on)))

;code that doesn't work but should
;(define-generator (rotate . args)
;        (let loop ()
;          (for-each yield args)
;          (loop)))
;
;(define traffic-light (rotate 'red 'yellow 'green)))


(define-syntax (my-rev stx)
  (syntax-case stx ()
    ((_ e ... op)
     #`(op #,@(reverse (syntax->list #'(e ...)))))))


;(check-macro '(my-rev 1 2 +))


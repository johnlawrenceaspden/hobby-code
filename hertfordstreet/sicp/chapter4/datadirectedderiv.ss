#lang scheme

(require (planet "main.ss" ("schematics" "schemeunit.plt" 3 3)))
(require (lib "trace.ss"))
(require scheme/help)
(require scheme/match)


(define (tagged-list? a l) (eq? (car l) a))

(define (variable? exp) (symbol? exp))
(define (same-variable? exp var) (eq? exp var))

(define (sum? exp) (tagged-list? '+ exp))
(define (make-sum . a )
  (cond [(null? a) (error "m+wtf")]
        [(null? (cdr a)) (car a)]
        [else (cons '+ a)]))
(define ( derive-sum operands var )
  (cond [(null? operands) (error "+wtf?")]
        [(null? (cdr operands)) (derive (car operands) var)]
        [else (make-sum (derive (car operands) var) (derive (apply make-sum (cdr operands)) var))]))

(define (product? exp) (tagged-list? '* exp))
(define (make-product . a )
  (cond [(null? a) (error "m*wtf?")]
        [(null? (cdr a)) (car a)]
        [else (cons '* a)]))
(define (derive-product operands var)
  (cond [(null? operands) (error "*wtf?")]
        [(null? (cdr operands)) (derive (car operands) var)]
        ;D (* a b c d ...) = (+ (* (D a) (* b c d ...)) (* a (D (* b c d ...)))
        [else (make-sum (make-product (derive (car operands) var) (apply make-product (cdr operands)))
                        (make-product (car operands) (derive (apply make-product (cdr operands)) var)))]))
 

(define (get symbol operator)
  (cond ((eq? symbol 'deriv)
         (cond ((eq? operator '+) derive-sum )
               ((eq? operator '*) derive-product)
               
               (else ((error (format "no ~a for ~a" operator symbol))))))))

(define (derive exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))



(define (simplify exp)
  (match exp
    ((list '+ (? number? a) ...) (apply + a))
    ((list '* (? number? a) ...) (apply * a))
    ((cons '* (list-no-order 0 a ...)) 0)
    ((list '+ x 0) (simplify x))
    ((list '+ 0 x) (simplify x))
    ((cons '+ (list-no-order 0 a ...)) (simplify (apply make-sum a))) 
    ((list '* 1 x) (simplify x))
    ((list '* x 1) (simplify x))
    ((list '+ x x) (list '* 2 (simplify x)))
    ((list '+ a ... (list '+ b ...)) `(+ ,@(map simplify a) ,@(map simplify b)))
    ((list '* a ... (list '* b ...)) `(* ,@(map simplify a) ,@(map simplify b)))
    ((list '* a ... (list '* b ...) c ...) `(* ,@(map simplify a) ,@(map simplify b) ,@(map simplify c)))
    ((list '* (? number? a) (? number? b) t ...) (cons '* (cons ( * a b) (simplify t))))
    ((list '* a ...) (cons '* (map simplify (sort a (λ(a b) (string<? (format "~a" a)(format "~a" b)))))))
    ((list '+ a ...) (cons '+ (map simplify (sort a (λ(a b) (string<? (format "~a" a)(format "~a" b)))))))
    ((list a b ...) (cons a (map simplify b)))  
    (x x)))

(define (simplify-until-convergence exp)
  (let ([s (simplify exp)])
    (if (equal? s exp) 
        exp
        (simplify-until-convergence s))))

  
(define poly (make-sum 
              (make-product 1000 
                            (make-sum
                             (make-product 'x 'x 'x)
                             (make-product 3 'x 'x 'y)
                             (make-product 'x 'y 'y 3)
                             (make-product 'y 'y 'y)))
              (make-product 100 
                            (make-sum 
                             (make-product 'y 'y) 
                             (make-product 2 (make-product 'x 'y) )
                             (make-product 'x 'x)))
              (make-product 10 
                            (make-sum 'x 'y))
              1))

(define averyhardexpression '(/
 (-
  (/ (* (- K 1) (- K 2) (- K 3) (- K 4) K) 120)
  (+
   (* (- (/ (* (- K 1) (- K 2) K) 720)) (/ (* (+ -3 K) (+ -4 K)) 2))
   (* (/ (* (+ -1 K) (+ -2 K) (+ -3 K) (+ -4 K) K (+ 1 K)) 720) (/ 1 (+ 1 K)))
   (* (/ (* (+ -1 K) (+ -2 K) (+ -3 K) (+ -4 K) K) 120) (/ 1 2))
   (* (/ (* (+ -1 K) (+ -2 K) (+ -3 K) (+ -4 K)) 24) (/ K 12))
   0))
 (+ -4 K)))


(define (test D)
  (list
   ( D 1 'x )
   ( D 0 'x )
   ( D 0 'y )
   ( D 'x 'x )
   ( D 'x 'y )
   ( D 'y 'x )
   ( D 'y 'y )
   ( D (make-sum 'x 'x) 'x )
   ( D (make-sum 'x 'y) 'x )
   ( D (make-sum 'y 'x) 'x )
   ( D (make-sum 'y 'y) 'x )
   ( D (make-sum 'x 'x) 'y )
   ( D (make-sum 'x 'y) 'y )
   ( D (make-sum 'y 'x) 'y )
   ( D (make-sum 'y 'y) 'y )
   ( D (make-product 'x 'x) 'x )
   ( D (make-product 'x 'y) 'x )
   ( D (make-product 'y 'x) 'x )
   ( D (make-product 'y 'y) 'x )
   ( D (make-product 'x 'x) 'y )
   ( D (make-product 'x 'y) 'y )
   ( D (make-product 'y 'x) 'y )
   ( D (make-product 'y 'y) 'y )
   ( D poly 'x)
   ( D poly 'y)
   ( D (D poly 'x ) 'x)
   ( D (D poly 'x) 'y )
   ( D (D poly 'y) 'x )
   ( D (D poly 'y) 'y )))
  
  
;(for ((i (test list))) (printf "~a d~a ->"(car i) (cadr i)) (printf " ~a~n" (derive (car i) (cadr i))))  
(map (λ(a b)(list a '-> b)) (test (λ(x v) (list 'd x (string->symbol (format "/d~a" v))))) (map simplify-until-convergence (test derive)))
(map (λ(a b)b) (test (λ(x v) (list 'd x (string->symbol (format "/d~a" v))))) (map simplify-until-convergence (test derive)))

(trace derive simplify simplify-until-convergence)
;(simplify-until-convergence '(+ y (+ (* 2 x) 4)))
;(test data-directed-deriv)
;(equal? (test data-directed-deriv) (test deriv))
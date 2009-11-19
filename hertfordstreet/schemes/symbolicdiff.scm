(load "infixtoscheme.scm")

(define (debugderive exp var)
  (display "exp ") (display exp) (newline)
  (let ((d (sderive exp var)))
    (display d)
    (newline)
    d))


(define (derive exp var)
  (cond ((const? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp) (makesum (derive (firstarg exp) var) (derive (restargs exp) var)))
        ((difference? exp) (makediff (derive (a1 exp) var ) (derive (a2 exp) var)))
        ((product? exp) (makesum (makeproduct (derive (firstarg exp) var) (restargs exp))
                                 (makeproduct (derive (restargs exp) var) (firstarg exp))))
        ((quotient? exp) (makequotient
                          (makediff (makeproduct (derive (a1 exp) var) (a2 exp))
                                    (makeproduct (derive (a2 exp) var) (a1 exp)))
                          (makeproduct (a2 exp) (a2 exp))))
        ((sine? exp) (makeproduct (makecosine (a1 exp))(derive (a1 exp) var)))
        ((cosine? exp) (makeproduct (makeunarydiff (makesine (a1 exp))) (derive (a1 exp) var))) 
        ((unarydifference? exp) (makeunarydiff (derive (a1 exp) var)))
        ((exponentiation? exp) (makeproduct (makeproduct (a2 exp) (makeexponentiation (a1 exp) (- (a2 exp) 1))) (derive (a1 exp) var)))
        (else (error "unknown operator" exp))
        ))


(define (firstarg sum) (cadr sum))
(define (restargs sum) (if (= (length sum) 3) (caddr sum) (append (list (car sum)) (cddr sum))))

(define (a1 exp) (cadr exp))
(define (a2 exp) (caddr exp))

(define (atom? exp) (not (pair? exp)))

(define (composition? exp proc)
  (and (not (atom? exp))
       (eq? (car exp) proc)))

(define (quotient? exp) (composition? exp '/))
(define (difference? exp) (and (composition? exp '-) ( = (length exp) 3)))
(define (unarydifference? exp) (and (composition? exp '-) (= (length exp) 2)))
(define (sum? exp) (composition? exp '+))
(define (product? exp) (composition? exp '*))
(define (sine? exp) (composition? exp 'sin))
(define (cosine? exp) (composition? exp 'cos))
(define (exponentiation? exp) (composition? exp '**))

(define (var? exp) (and (atom? exp) (not (number? exp))))

(define (same-var? exp var)
  (and (atom? exp) 
       (eq? exp var)))

(define (const? exp var)
  (and (atom? exp)
       (not(eq? exp var))))

(define (makecomposition proc a b)
  (list proc a b))


(define (makesum a b)
  (cond 
        ((and (number? a) (= a 0)) b)
        ((and (number? b) (= b 0)) a)
        ((and (number? a) (number? b)) (+ a b))
        ((and (var? a) (same-var? a b) (makeproduct 2 a)))
        ((and (sum? a) (atom? b) ( append a (list b))))
        ((and (atom? a) (sum? b)) (append b (list a)))
        ((and (sum? a) (sum? b)) (append a (cdr b)))
        (else (makecomposition '+ a b))))

(define (makediff a b)
  (cond ((and (number? a) (= a 0)) (makeunarydiff b))
        ((and (number? b) (= b 0)) a)
        ((and (number? a) (number? b)) (- a b))
        (else (makecomposition '- a b)))) 

(define (makeproduct a b)
  (cond 
        ((and (number? a) (number? b)) (* a b))
        ((and (number? a) (= a 1)) b)
        ((and (number? b) (= b 1)) a)
        ((and (number? a) (= a 0)) 0)
        ((and (number? b) (= b 0)) 0)
        ((and (product? a) (atom? b) ( append a (list b))))
        ((and (atom? a) (product? b)) (append b (list a)))
        ((and (product? a) (product? b)) (append a (cdr b)))((and (number? a) (number? b) (* a b)))
        (else (makecomposition '* a b))))

(define (makequotient a b)
  (cond ((and (number? b) (= b 1)) a) 
        ((and (number? a) (= a 0)) 0)
        (else (makecomposition '/ a b))))

(define (makeexponentiation a b)
  (cond ((and (number? b) (= b 1) a))
        ((and (number? b) (= b 0) 1))
        (else (makecomposition '** a b))))

(define (makecosine a) (list 'cos a))
(define (makesine a) (list 'sin a))

(define (makeunarydiff exp) 
  (cond ((number? exp) (- exp))
        (else (list '- exp)))) 


(define ax2bxc '(+ (* a (* x x))
                (* b x)
                c))

(define (symbolicdifftest expression variable answer)
  (let ((deriv (derive expression variable)))
    (if (equal? deriv answer)
        #t
        (begin
        (display "test failed: ") (newline)
        (display "expression :")(display expression) (display " differentiated w.r.t. ") (display variable) 
        (display " was ") (display deriv)
        (display " <> ") (display answer)(newline)
        #f))))



(if (and (symbolicdifftest '1                   'x                 0)
         (symbolicdifftest 'x                   'x                 1)
         (symbolicdifftest '(+ x x)             'x                 2)
         (symbolicdifftest '(+ x (* 2 x))       'x                 3)
         (symbolicdifftest '(- 1 x)             'x                -1)
         (symbolicdifftest '(/ 1 x) 'x           '(/ -1 (* x x)) )
         (symbolicdifftest '(sin x) 'x           '(cos x))
         (symbolicdifftest '(cos x) 'x           '(- (sin x)))
         (symbolicdifftest '(- (* x x)) 'x       '(- (* 2 x)))
         (symbolicdifftest ax2bxc 'x             '(+ (* 2 x a) b))
         (symbolicdifftest ax2bxc 'y             0)
         (symbolicdifftest ax2bxc 'a             '(* x x))
         (symbolicdifftest ax2bxc 'b             'x)
         (symbolicdifftest ax2bxc 'c             1)
         (symbolicdifftest '(sin (* x 2)) 'x     '(* (cos (* x 2))2 ))         
         (symbolicdifftest '(cos (sin x)) 'x     '(*(- (sin (sin x))) (cos x)))
         (symbolicdifftest '(** (sin x) 5) 'x    '(* (* 5 (** (sin x) 4)) (cos x)) )
         (symbolicdifftest '(+ x x x) 'x         3)
         (symbolicdifftest '(+ x x y x) 'y       1)
         (symbolicdifftest '(* x x x) 'x         '(+ (* x x) (* 2 x x)) )
         (symbolicdifftest '(* x x y x) 'y       '(* x x x) )
         (symbolicdifftest '(/ y x) 'y           '(/ x (* x x)) )
         (symbolicdifftest '(/ y x) 'x           '(/ (- y) (* x x)) )
         )
    (display "tests passed")
    (display "tests failed"))

(define (d/dx s) (derive s 'x))
(define (d2/dx2 s) (d/dx (d/dx s)))

(d/dx '(* x(* x (* x(* x x)))) )
(d2/dx2  '(* x(* x (* x(* x x)))) ) 

(d/dx '(** x 5))
(d/dx (d/dx (d/dx (d/dx (d/dx '(** x 5))))))




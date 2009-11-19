;substitution evaluations of square 2 in various ways.

(define (square x) (* x x))  
(square 2)
(* 2 2)
4

(define square (lambda (x)(* x x)) )
(square 2)
( (lambda (x) (* x x)) 2 )
              (* 2 2)     
(* 2 2)
4

(define ((op2 op) x) (op x x))
((op2 *) 2)
(* 2 2)

(define (op2 op) (lambda (x) (op x x)) )
((op2 *) 2)
((lambda (x) (* x x)) 2)
             (* 2 2) 
             
(define op2 (lambda (op) (lambda (x) (op x x))))
((op2 *) 2)
( ((lambda (op) (lambda (x) (op x x)) ) * )  2)
(               (lambda (x) (*  x x))        2)
                            (*  2 2)           
                            

(define zero 
  (lambda (f) (lambda (x) x))
  ) ;zero: f-> i: x->x

(define (add-1 n) 
  (lambda (f) (lambda (x) (f ((n f) x)))) ) ;add-1(n) g: f-> q: x-> (f(n(f(x))) )


(zero square)

((lambda (f) (lambda (x) x)) square)

             (lambda (x) x) 
             
(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))

((add-1 zero) square)
((lambda (f) (lambda (x) (f ((zero f) x)))) square)
             (lambda (x) (square ((zero square) x))) 
             (lambda (x) (square (((lambda (f) (lambda (x) x)) square) x))) 
             (lambda (x) (square (             (lambda (x) x)          x))) 
             (lambda (x) (square ((lambda (x) x) x))) 
             (lambda (x) (square              x    )) 
             (lambda (x) (square x))
             
 (add-1 zero)
 (lambda (f) (lambda (x) (f x)))
 
 
(define one  (lambda (f) (lambda (x) (f x))) )

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f (             (lambda (x) (f x))     x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x))x))))
(lambda (f) (lambda (x) (f              (f x)   )))
(lambda (f) (lambda (x) (f(f x))))

(define two (lambda (f) (lambda (x) (f(f x)))) )

(add-1 two)
(lambda (f) (lambda (x) (f ((two f) x))))
(lambda (f) (lambda (x) (f ((two f) x))))


; zero is f->i
; one is f->f
; two is f->f^2
; three is f->f^3


"-------" 
(define (square x) (* x x))
(define one (add-1 zero))
(define two (add-1 one))
(define three (add-1 two))
(define four (add-1 three))
(define five (add-1 four))
(define six (add-1 five))
(define seven (add-1 six))
(define eight (add-1 seven))
(define nine (add-1 eight))

"--------"
((zero square) 2) ; 2
((one square) 2)  ; 4
((two square) 2)  ; 16
((three square) 2); 256
((four square) 2) ; 65536
((five square) 2) ; 4294967296
((six square) 2)  ; 18446744073709551616
((seven square) 2) ; 340282366920938463463374607431768211456
((eight square) 2) ; 115792089237316195423570985008687907853269984665640564039457584007913129639936
((nine square) 2) ;1340.........
"------------"

(define (plus a b) (lambda (f) (lambda (x) ((a f)((b f) x))))) 

(((plus one one) square) 2) ;16
(((plus one two) square) 2) ;256
(((plus one three) square) 2) ; 65536
(((plus two one) square) 2) ; 256
(((plus two two) square) 2) ; 65536
(((plus two three) square) 2) ; 4294967296
(((plus three one) square) 2) ; 65536
(((plus three two) square) 2) ; 4294967296
(((plus three three) square) 2) ; 18446........

"---------"

(define (times a b) (lambda (f) (lambda (x) ((a (b f)) x))))

(((times one one) square) 2) ; 4
(((times one two) square) 2) ; 16
(((times one three) square) 2) ; 256
(((times two one) square) 2) ; 4
(((times two two) square) 2) ; 65536
(((times two three) square) 2) ; 18446......
(((times three one) square) 2) ; 256
(((times three two) square) 2) ; 18446......
(((times three three) square) 2) ; 1340......




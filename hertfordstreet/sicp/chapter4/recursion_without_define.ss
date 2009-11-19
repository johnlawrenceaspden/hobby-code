#lang scheme

;yet more ways to calculate factorial: the usual way

(define (factorial n)
  (if (< n 2) n (* n (factorial (- n 1)))))

(map factorial '(0 1 2 3 4 5 6 7 8 9 10))

;but there's something a bit fishy going on here. factorial is defined in terms of itself. 
;in my lexically scoped metacircular evaluator this didn't work, because the function was evaluated in the environment in which it was bound, which didn't contain factorial. 
;with a dynamic evaluator that's not a problem, because when the function is called, the binding is present in the global environment.
;However it does show that there's something special about recursive functions. They need to be allowed for specially
;It doesn't need to be:

;consider 
(define (loop1) (loop1))
;the simplest possible loop

;we can do this without using a recursive definition
(define (loop2 f) (f f))
; to make a loop we call (loop2 loop2), which never terminates. Thus the ability of a function to take itself as an argument allows us recursion. We don't need
; witchy defines



;yet more ways to calculate factorial: without using recursive definition

(define (loop f n) (if (< n 2) n (* n (f f (- n 1)))))

(map (λ(n) (loop loop n)) '(0 1 2 3 4 5 6 7 8 9 10))

;yet more ways to calculate factorial: pure lambda calculus without any defines

;(factorial 0)

((lambda (n) ((lambda (fact) (fact fact n)) (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))))) 0)

((lambda (fact) (fact fact 0)) (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))))

((lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))) (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))) 0)

(cond ((< 0 2) 0) (else (* 0 ((lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1))))))(lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))) (- 0 1)))))

0

;(factorial 1)

((lambda (n) ((lambda (fact) (fact fact n)) (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))))) 1)

;substitute 1 for n

((lambda (fact) (fact fact 1)) (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))))

;substitute (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))) for fact

((lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))) (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))) 1)

((lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))) (lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))) 0)

(cond ((< 0 2) 0) (else (* 0 ((lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1))))))(lambda (ft k) (cond ((< k 2) k) (else (* k (ft ft (- k 1)))))) (- 0 1)))))

0
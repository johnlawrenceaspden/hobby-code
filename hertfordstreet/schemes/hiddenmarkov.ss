#! /usr/bin/env mzscheme
#lang scheme

;hidden markov model

(define (mm-next-state random state)
  (match state 
    ('rainy (cond ((< random 0.7) 'rainy)
                  (else 'sunny)))
    ('sunny (cond ((< random 0.4) 'rainy)
                  (else 'sunny)))))


(define (mm-sequence random-list start)
  (if (null? random-list) (list start)
      (let ((seq (mm-sequence (cdr random-list) start)))
        (cons (mm-next-state (car random-list) (car seq)) seq))))

(define (random-list length seed)
  (random-seed seed)
  (for/list ((i (in-range length))) (random)))

;dict={}
;for x in (rainy sunny sunny):
;   dict(x)++



(define (count-eq seq)
  (for/fold ((dict #hasheq())) ((i seq))
    (hash-set dict i (+ 1 (hash-ref dict i 0)))))

(count-eq (mm-sequence (random-list 100 1) 'rainy))




;(freq-functional (mm-sequence (random-list 100 1) 'rainy))


;trying to construct a graph with cycles to represent a markov chain.

(define-struct state (name transitions outputs) #:transparent)

(define-struct transition (probability state) #:transparent #:mutable)

(define-struct output (probability output))

;we can use quotes, but then we have to eval them 
(define x1 (make-state "x1" (list (make-transition 0.5 'x1) (make-transition 0.5 'x2)) (list (make-output 1 "X1" ))))
;we can use lambdas, which then need forcing
(define x2 (make-state "x2" (list (make-transition 0.5 (λ()x1)) (make-transition 0.5 (λ()x3))) (list (make-output 1 "X2" ))))


; or we can use mutation
(define x3 (make-state "x3" (list (make-transition 1   'x3)) (list (make-output 1 "X3"))))
(set-transition-state! (car (state-transitions x3)) x3)

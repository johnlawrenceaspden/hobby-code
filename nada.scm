#lang racket

;; http://io.livecode.ch/learn/gregr/icfp2017-artifact-auas7pp

(cdr '(Hark! I love you))

((lambda (a c b) (list a b c)) 'I 'you 'love)

(match #t [#t '(I love you)][#f "boo"])

(define append 
  (lambda (l s)
    (cond 
      [(null? l) s]
      [else (cons (car l) (append (cdr l) s))])))

(append '(a b c) '(d e f))

(define append1 
  (lambda (l s)
    (cond 
      [(null? l) s]
      [else (let* ((a (car l))
                   (b (cdr l))
                   (res (append1 b s)))
              (cons a res))])))

(append1 '(a b c) '(d e f))

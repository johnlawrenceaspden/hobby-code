#lang icfp2017-minikanren/racket-scheme-compat

;; minikanren tutorial 
;; http://io.livecode.ch/learn/webyrd/webmk

(require "mk/mk.rkt")


(define append-0
  (lambda (l s)
    (cond
      [(null? l) s]
      [else (cons (car l) (append-0 (cdr l) s))])))

; (append-0 '(a b) '(b c))

(define append
  (lambda (l s)
    (cond
      [(null? l) s]
      [else
       (let* ((a (car l))
              (d (cdr l))
              (res (append d s)))
         (cons a res))])))


; (append '(a b) '(b c))


(define appendo
  (lambda (l s ls)
    (conde
      [(== '() l) (== s ls)]
      [(fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) ls)
         (appendo d s res))])))

(print '(run 1 (q) (appendo '(a) '(b) '(a b)))) (print '->)
(run 1 (q) (appendo '(a) '(b) '(a b)))

;l = '(a)
;s = '(b)
;ls = '(a b)
;
;l=='() FAIL
;
;are there:  a d res
;s.t.
;
;a d == '(a)
;a res == '(a b)
;and append d '(b) is res
;
;a = '(a)
;d = '()
;res = b FAIL
;
;a = ()
;d = '(a)
;=> 
;res = '(a b)
;SUCCESS





(run 1 (q) (appendo '(a) '(b) q))
(run 1 (q) (appendo q '(b) '(a b)))
(run 1 (q) (appendo '(a) q '(a b)))






(run 1 (q) (fresh (x y z) (== x z) (== 3 y)))




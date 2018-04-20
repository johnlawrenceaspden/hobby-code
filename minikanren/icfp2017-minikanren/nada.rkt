#lang icfp2017-minikanren/racket-scheme-compat

;; Install the racket package icfp2017-minikanren
;; and then copy its files around this file to get it to work
;; cp -a ~/.racket/6.7/pkgs/icfp2017-minikanren/ ~/hobby-code/minikanren/

;; There is likely a better way!

;; minikanren tutorial 
;; http://io.livecode.ch/learn/webyrd/webmk

(require "mk/mk.rkt")
(require "evalo-standard.rkt")


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


;; input l s ls
;; can succeed if l is empty and s and ls are the same
(run* (q) (appendo '() '(a b) '(a b)))
;; if l is empty and either s or ls are variable then can succeed almost immediately
(run* (q) (appendo '() q '(a b)))
;; just becomes:
(run* (q) (== q '(a b)))

;; if l is not empty
(run* (q) (appendo '(a) q '(a b)))

;; then we're all
(run* (q) (fresh (alpha beta res)
                 (== `(,alpha . ,beta) '(a))
                 (== `(,alpha . ,res) '(a b))
                 (appendo beta q res)))

;;; can we find alpha so that
;alpha + beta is '(a)
;alpha + res is '(a b)
;beta + q is res







;; otherwise

;
;
;
;(printf "(run 1 (q) (appendo '(a) '(b) '(a b)))) -> ")
;
;(run 1 (q) (appendo '(a) '(b) '(a b)))
;
;
;
;;l = '(a)
;;s = '(b)
;;ls = '(a b)
;;
;;l=='() FAIL
;;
;;are there:  a d res
;;s.t.
;;
;;a d == '(a)
;;a res == '(a b)
;;and append d '(b) is res
;;
;;a = '(a)
;;d = '()
;;res = b FAIL
;;
;;a = ()
;;d = '(a)
;;=> 
;;res = '(a b)
;;SUCCESS
;
;
;
;
;
;(run 1 (q) (appendo '(a) '(b) q))
;(run 1 (q) (appendo q '(b) '(a b)))
;(run 1 (q) (appendo '(a) q '(a b)))
;
;
;
;
;
;
;(run 1 (q) (fresh (x y z) (== x z) (== 3 y)))
;
;
;


(run* (x y) (appendo x y '(a b c d e)))



(run 1 (q) (evalo '((lambda (x) x) 5) q))
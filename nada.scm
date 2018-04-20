#lang racket

(require minikanren)

;; #lang icfp2017-minikanren
;; #lang icfp2017-minikanren/racket-scheme-compat

;; http://io.livecode.ch/learn/gregr/icfp2017-artifact-auas7pp

;; https://pkgs.racket-lang.org/package/icfp2017-minikanren

;; Install minikanren package File->Install Package in Dr Racket

;;(require icfp2017-minikanren)



(cdr '(Hark! I love you))

((lambda (a c b) (list a b c)) 'I 'you 'love)

(match #t [#t '(I love you)][#f "boo"])


;; append as normally written
(define append 
  (lambda (l s)
    (cond 
      [(null? l) s]
      [else (cons (car l) (append (cdr l) s))])))

(append '(a b c) '(d e f))

;; append slightly restructured
(define append1 
  (lambda (l s)
    (cond 
      [(null? l) s]
      [else (let* ((a (car l))
                   (b (cdr l))
                   (res (append1 b s)))
              (cons a res))])))

(append1 '(a b c) '(d e f))

;; minikanren version
(define appendo
  (lambda (l s ls)
    (conde 
     [ (== '() l) (== s ls)]
     [ (fresh (a d res)
              (== `(,a . ,d) l)
              (== `(,a . ,res) ls)
              (appendo d s res))])))


(append '(a b c) '(d e))
(run* (q) (appendo '(a b c) '(d e) q))

(run* (x) (appendo x x '(a b c a b c)))



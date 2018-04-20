#lang icfp2017-minikanren/racket-scheme-compat

;; minikanren tutorial 
;; http://io.livecode.ch/learn/webyrd/webmk

(require "mk/mk.rkt")

(run 1 (q) (fresh (x y z) (== x z) (== 3 y)))

(run 1 (y)
  (fresh (x z)
    (== x z)
    (== 3 y)))

(run 1 (q)
     (fresh (x y)        
            (== y x)
            (== y q)
            (== x 3)))


(run 1 (y)
  (fresh (x y)
    (== 4 x)
    (== x y))
  (== 7 y))

(run 1 (x) (== 4 3))

(run 1 (x) (== 5 x) (== 6 x))

(run 2 (q)
  (fresh (w x y)
    (conde
      ((== `(,x ,w ,x) q)
       (== y w))
      ((== `(,w ,x ,w) q)
       (== y w)))))

(run 5 (q)
     (let loop ()
     (conde ((== #f q))
            ((== #t q))
            ((loop)))))



(run 5 (q)
     (conde ((== #f q))
            ((== #t q))
            ((== #f q))))


(run 10 (q)
     (conde ((== #f q))
            ((== #t q))
            ((conde ((== #f q))
                    ((== 'yo q))
                    ((conde ((== #f q) (== #t q))
                            ((== 'oy q))))))))


(define anyo
  (lambda (g)
    (conde
     (g)
     ((anyo g)))))

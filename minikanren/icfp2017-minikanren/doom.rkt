#lang icfp2017-minikanren/racket-scheme-compat

(require "mk/mk.rkt" "mk/test-check.rkt")

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
  (== 3 y))


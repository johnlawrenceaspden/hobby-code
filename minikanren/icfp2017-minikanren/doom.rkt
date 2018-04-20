#lang icfp2017-minikanren/racket-scheme-compat

(require "mk/mk.rkt" "mk/test-check.rkt")

(load "mk/test-check.scm")
(load "mk/mk.scm")



(run 1 (q) (fresh (x y z) (== x z) (== 3 y)))

(run 1 (y)
  (fresh (x z)
    (== x z)
    (== 3 y)))


;;(include "intro-examples.scm")
;;(declare-racket-wrapper-for "intro-examples.scm")

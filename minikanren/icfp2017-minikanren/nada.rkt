#lang icfp2017-minikanren/racket-scheme-compat

;; minikanren tutorial 
;; http://io.livecode.ch/learn/webyrd/webmk

(require "mk/mk.rkt")

(run 1 (q) (fresh (x y z) (== x z) (== 3 y)))





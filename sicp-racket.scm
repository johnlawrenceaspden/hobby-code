; there's a sicp compatibility module in racket, install sicp in the package manager 'do what I mean' tab

#lang sicp

(identity (inc 42))

;; and the sicp-pict module gives you the picture language and amb and cons-stream too
(#%require sicp-pict)

(amb 3 2)

(paint einstein)
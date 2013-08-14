;; The Shield of Athena

;; It's a perennial problem for me that occasionally I ask my REPL for an infinite sequence.

;; never do this: 
(iterate (fn[s] (list s s)) '()) ;-> death

;; It's not so bad in a terminal, where the REPL just freezes, but in emacs it's a disaster.
;; The whole thing snarls up in a really nasty way and it usually takes several minutes to fix.

;; A real man would dive into the nrepl code and fix it, but life is too short.

;; safety catch on
(set! *print-length* 7)
(set! *print-level* 7)

(iterate (fn[s] (list s s)) '()) ;-> (() (() ()) ((() ()) (() ())) (((() ()) (() ())) ((() ()) (() ()))) ((((() ()) (() ())) ((() ()) (() ()))) (((() ()) (() ())) ((() ()) (() ())))) (((((() ()) (() ())) ((() ()) (() ()))) (((() ()) (() ())) ((() ()) (() ())))) ((((() ()) (() ())) ((() ()) (() ()))) (((() ()) (() ())) ((() ()) (() ()))))) ((((((# #) (# #)) ((# #) (# #))) (((# #) (# #)) ((# #) (# #)))) ((((# #) (# #)) ((# #) (# #))) (((# #) (# #)) ((# #) (# #))))) (((((# #) (# #)) ((# #) (# #))) (((# #) (# #)) ((# #) (# #)))) ((((# #) (# #)) ((# #) (# #))) (((# #) (# #)) ((# #) (# #)))))) ...)

;; of course there is a price to pay

(range 10) ;-> (0 1 2 3 4 5 6 ...)

((apply comp (repeat 10 list)) 'hi) ;-> (((((((#)))))))

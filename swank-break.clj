;; All power to:

;; http://hugoduncan.org/post/2010/swank_clojure_gets_a_break_with_the_local_environment.xhtml

;; Who has given us a way to debug things under emacs/slime/swank, by stopping a
;; running program to examine the variables

;; First define this function under emacs/slime/swank

(defn factorial [n]
  (when (= n 23) (swank.core/break))
        (if (< n 2) n
            (* n (factorial (dec n)))))

;; Then try 
(factorial 30)
;; at the repl, so it runs in the repl thread, rather than executing it with
;; C-M-x or C-x e, which for some reason doesn't work as well.

;; Hugo's article explains how to view the local environment and create a repl
;; in context so that you can examine the state of the program when break was
;; called. You can then restart as if nothing had happened.

;; It's not as wonderful as traditional SLIME/LISP debugging, but it's a good 
;; start!



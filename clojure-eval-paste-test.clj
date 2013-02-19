;; Clojure Emacs Eval Paste Keyboard Macros / 
;; Generating your Regression Tests Automatically while Writing your Functions

;; There's a phenomenally useful feature of emacs when using clojure
;; and nrepl where you can evaluate the result of an
;; expression and paste the result directly into a buffer.

;; However my attempts to use it in keyboard macros have always led to frustration.

;; I asked about it on stack overflow:
;; http://stackoverflow.com/questions/14959155/how-can-i-make-emacs-keyboard-macros-work-properly-when-pasting-the-results-of-c#14960321

;; and a nice chap called sds helped me come up with the following work-around:

;; Say I've defined the function:

(defn << [a b]
  (if (zero? b) a
      (<< (* 2 a) (dec b))))


;; And now I want to check that it works:

;; I evaluate the following two forms in emacs lisp (in the *scratch* buffer)

(defun clojure-eval-paste-test ()
  (interactive)
  (next-line)
  (move-beginning-of-line nil)
  (insert "(is= ")
  (forward-sexp)
  (insert " ")
  (nrepl-eval-last-expression 't)
  (sleep-for 0.1)
  (insert ")"))

(global-set-key [f5] #'clojure-eval-paste-test)


;; Now I type:

(<< 1 0)
(<< 1 1)
(<< 1 2)
(<< 2 0)
(<< 2 2)
(<< 2 3)

;; And then I move point to just above the first line, and press f5 six times, and I get:


(is= (<< 1 0) 1)
(is= (<< 1 1) 2)
(is= (<< 1 2) 4)
(is= (<< 2 0) 2)
(is= (<< 2 2) 8)
(is= (<< 2 3) 16)

;; Which both reassures me that it does work, and provides the skeleton of a regression test for it!



;; There are more elegant ways of doing this, presumably, one being to
;; do as sds suggests, and code up the function using lower level
;; calls rather than the (sleep-for 0.1) thing that will obviously go
;; wrong if the eval takes too long.

;; And another way would be to fix it so that nrepl-eval-last-expression actually works in keyboard macros. 

;; But this will do for now, and gets rid of a papercut bug that's been annoying me for ages.
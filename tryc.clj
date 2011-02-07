;; I often find that the default behaviour of exceptions in emacs/slime is a bit
;; annoying.

;; Up pops up a debugger window, but that moves the focus away from where
;; you're working, and the new window then needs dismissing, and has often
;; buggered up the size of other windows, etc...

;; Also when playing with experimental clojure versions, the behaviour is a bit
;; random, and often the expression just gets swallowed, with no clue as to what
;; it was:

;; Anyway I wrote this little macro, which turns an exception into a return value:
(defmacro tryc[ & e]
  `(try ~@e
       (catch Exception a# a#)))


(tryc (java.io.File ".")) ;; #<ClassCastException java.lang.ClassCastException: java.lang.Class cannot be cast to clojure.lang.IFn>

;; Obviously you should only use this at the top level!

;; I find I'm using it all the time when writing code in emacs


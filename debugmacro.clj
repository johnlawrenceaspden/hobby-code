;;Having thoroughly embarrassed myself recently by being unable to get a simple debug macro working
;;whilst trying to demonstrate how simple and useful they are, I thought I'd write the example down.

;;suppose we have a simple program:

(def a 10)
(def b 20)

(println "answer:" (* a b))

;; suppose we want to debug it. We may wish to insert tracing statements, that print out the 
;; values of various expressions as the program runs

;These tend to look like:
(println "the current value of a is" a)
;or equivalently:
(println "the current value of" (quote a) "is" a)
;this rapidly gets old, particularly if what we want is to look at the values of complex expressions
(println "the current value of" (quote (* a b)) "is" (* a b))

;we can make the repeated text go away with a function
(defn debug-fn [exp val]
  (println "the current value of" exp "is" val))

;which allows us to write
(debug-fn '(* a a) (* a a))
;but that's as far as it goes. We still need to write the expression twice, and we still need
;the ', or (quote ) to stop the literal expression from getting evaluated.
;even if we're prepared to use run-time evaluation, we have to jump through hoops to make 
;sure that the expression gets evaluated in the correct environment. Which is worse.
;this drives me up the wall in every non-lisp language. It's such a useful construct that I'm 
;surprised that there isn't a special way to do it. 

;what would be nice, is if we could say to the compiler
; every time you see something like 
; (debug (* a a))
; imagine I'd written 
; (println "the current value of" (quote (* a a)) "is" (* a a))
; instead.

;and in a lisp, we can: 
(defmacro debug [var]
  `(println "the current value of " (quote ~var) "is" ~var))

;here are some examples
(debug *)
(debug a)
(debug b)
(debug (* a b))
(debug (* 1 2 3 4))

;we can ask the compiler exactly what it pretends to see when it sees (debug a)
(println (macroexpand '(debug a)))
;or we could be smug:
(debug (macroexpand '(debug a)))




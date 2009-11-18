;macros 101 part 2. Classic sins and how to avoid them.
;here's our basic debug macro
(defmacro debug [var]
  `(println "the current value of" '~var "is" ~var))

;here's an example
(def a (* 5 5))
(debug a)

;at present, the value of the debug expression itself is nil, coming from the println
(debug (debug a))

;we could improve it by returning the value of the expression. That would let us use it on 
;subexpressions in larger programs 

(defmacro debug [var]
  `(do
     (println "the current value of" '~var "is" ~var)
     ~var))

;that works well
(+ (* 5 5) (debug (* 6 6)))

;but in fact we have a problem, as shown when we evaluate an expression with side effects.
(debug (println "hello"))

;hello gets printed twice because the compiler sees the expression twice. 
(macroexpand '(debug (println "hello")))

;to avoid this, we can use a temporary variable in our macro.
(defmacro debug [var]
  `(let [oops ~var] 
     (do
       (println "the current value of" '~var "is" oops)
     oops)))

;this expands as we would like, except that the temporary variable has been resolved into
;the current namespace
(macroexpand '(debug (println "hello")))

;which means that 
(debug (println "hello"))
;fails because you can't use let on a global binding.

;clojure has a clever way to make local variables in macros. You end them with a #....
(defmacro debug [var]
  `(let [oops# ~var] 
     (do
       (println "the current value of" '~var "is" oops#)
     oops#)))

;the new macro works as expected
(debug (println "hello"))
(println (debug (+ (debug (* 1 2)) (debug (* 2 3)))))

;macroexpand will show the auto-generated symbol names
(macroexpand '(debug (+ (debug (* 1 2)) (debug (* 2 3)))))



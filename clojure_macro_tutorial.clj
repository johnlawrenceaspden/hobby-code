;; A couple of chaps have asked me to write a clojure macro tutorial, to explain
;; my debugging macro

;; I'm going to try to imagine that I didn't know how to write dbg, and had to
;; go at it by trial and error, to show why it is as it is.

;; I think that the best way to learn to do something is by going through lots
;; of examples. I haven't followed this path, so I imagine that no-one is going
;; to learn how to write macros from following this through.

;; But theory helps too. I'd like to try to communicate the why of macros.

;; You should find lots of examples too. I'll keep an eye out for some simple
;; ones to use in a follow up set of exercises.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The problem:

;; Often the best way to debug code is by adding print statements.

;; Consider our old friend factorial

(defn factorial [n]
  (if (< n 2) n
      (* n (factorial (dec n)))))

(factorial 5) ;; 120

;; How would we watch it at work?

(defn factorial [n]
  (if (< n 2) n
      (* n (let [a (factorial (dec n))]
             (println "(factorial (dec n))=" a)
             a))))

(factorial 5)
;;(factorial (dec n))= 1
;;(factorial (dec n))= 2
;;(factorial (dec n))= 6
;;(factorial (dec n))= 24
;;120

;; So now we can watch the stack unwind. This gives us confidence in the inner
;; workings of the function.

;; The problem is that I've had to do a fair bit of typing to change the function
;; into a version that prints out its intermediate values.

;; Here's the original function again (re-evaluate the definition)
(defn factorial [n]
  (if (< n 2) n
      (* n (factorial (dec n)))))

(def n 5) ;; and let's give n a value so that fragments will run

;; Specifically, what I had to do was change
(factorial (dec n))
;; into
(let [a (factorial (dec n))] (println "(factorial (dec n))=" a) a)
;; Which is an expression which not only evaluates to 5, like the original did
;; but prints out (factorial (dec n))=5 at the same time as a side effect.

;; Any time you find that you have to do too much typing, you will also find
;; difficulty in reading, and potential for error. It is always to be avoided.

;; This simple repetitive task should be as easy as changing
(factorial (dec n))
;; to
(dbg (factorial (dec n)))
;; or even (dbg factorial (dec n)), now I come to think about it!

;; Normally, when spotting a common pattern like this, one makes a function.
;; But a function to do what we want is problematical.

;; We need the source code as well as the evaluated value of (factorial (dec n))
;; Something like:
(defn dbgf [s x]
  (println s "=" x)
  x)

(defn factorial [n]
  (if (< n 2) n
      (dbgf "(* n factorial(dec n))" (* n (factorial (dec n))))))
  
;; That's certainly better, but I'm still changing
(* n (factorial (dec n)))
;; into 
(dbgf "(* n factorial(dec n))" (* n (factorial (dec n))))

;; Which is less error prone, but still repetitive.

;; The reason that we need to hold dbgs hand like this, telling it what to
;; print out in two different ways, is that a function's arguments need to be evaluated before the function is called.

;; If we want to write
(dbg (* n (factorial (dec n))))
;; and have it work, then we need to take control of when
;; (* n (factorial (dec n))) is evaluated.

;; And this is the problem that macros solve.

;; We need to work out how to write:
(dbg (* n (factorial (dec n))))
;; And get:
(let [a (factorial (dec n))] (println "(factorial (dec n))=" a) a)

;; Now we can generate that code with an ordinary function, if we call it with
;; the code to be transformed rather than the value of the expression:

;; Let's define:
(defn dbg-code [s]
  (list 'let ['a s] (list 'println (list 'quote s) "=" 'a) 'a))
;; Which is just a function that takes a tree of symbols and gives back another
;; tree of symbols.

(dbg-code '(* n (factorial (dec n))))
;; (let [a (* n (factorial (dec n)))] (println (quote (* n (factorial (dec n)))) = a) a)

;; Nothing 'macro' has gone on yet! This is just a function, taking advantage of
;; lisp's ability to easily deal with the lists and trees and vectors that make
;; up lisp code.
(defmacro dbg-1 [s]
  (list 'let ['a s] (list 'println (list 'quote s) "=" 'a) 'a))

;; Now it's a macro!

;; Let's try it out:
(defn factorial [n]
  (if (< n 2) n
      (dbg-1 (* n (factorial (dec n))))))

(factorial 5)
;; (* n (factorial (dec n))) = 2
;; (* n (factorial (dec n))) = 6
;; (* n (factorial (dec n))) = 24
;; (* n (factorial (dec n))) = 120
;; 120

;; Bingo!

;; When the compiler sees a macro, which is just a function that returns some
;; code, it runs the function, and substitutes the new code for the macro.

;; It is like programming the compiler to be an apprentice programmer who will
;; write out the tedious bits longhand for you.

;; We can even ask the compiler what it sees when it expands dbg-1:
(macroexpand-1 '(dbg-1 x))       ;;(let [a x] (println (quote x) "=" a) a)
(macroexpand-1 '(dbg-1 (* x x))) ;;(let [a (* x x)] (println (quote (* x x)) "=" a) a)
(macroexpand-1 '(dbg-1 (println x))) ;; (let [a (println x)] (println (quote (println x)) "=" a) a)
(macroexpand-1 '(dbg-1 (inc x)))     ;;(let [a (inc x)] (println (quote (inc x)) "=" a) a)

;; So have we won??

;; We have certainly solved our problem as we originally conceived it, but there
;; are a couple of difficulties with our solution.

;; One is that when out macro is expanded, the names of functions may not be
;; what they were when it was defined. Consider the case when a macro is
;; exported from one namespace into another.

;; For instance, consider the case where we're defining a local version of println.
(defn println [& s]
  (apply clojure.core/println "eek:" s))

(dbg-1 n)
;;eek: n = 5
;;5

;; Now there will be times when you want this to happen, and times when you do not
;; and mostly you will not.

;; So a version of the macro where this doesn't happen is:
(defmacro dbg-2 [s]
  (list 'let ['a s] (list 'clojure.core/println (list 'quote s) "=" 'a) 'a))
;; explicitly specifying which namespace we want the function to come from.

(dbg-2 n)
;;n = 5
;;5

;; But there is a more subtle problem, to do with our slightly silly choice of
;; temporary variable.

;; If we tried to debug an expression which had an a in it, there would be
;; a certain confusion:
(macroexpand '(dbg-1 (* a a))) ;; (let* [a (* a a)] (println (quote (* a a)) "=" a) a)

;; I can't see a way in which this could cause a problem for dbg-2, it is just
;; bad style.

;; But in more complicated macros, the interference between the variables introduced by the macros and the variables in the source code can be a source of subtle bugs.

;; If we don't want them to interfere (and sometimes we do!) we'd be better off
;; choosing an unlikely name.

(defmacro dbg-3 [s]
  (list 'let ['unlikely s] (list 'clojure.core/println (list 'quote s) "=" 'unlikely) 'unlikely))

(macroexpand-1 '(dbg-3 (* a a)))
;;(let [unlikely (* a a)] (clojure.core/println (quote (* a a)) "=" unlikely) unlikely)

;; But this solution is also vulnerable, and however unlikely a name you pick,
;; there is always the chance that it will interfere with another name, especially when you start combining macros with other macros, which you will do (dbg-3 already depends on let, which is a macro itself.)

;; Luckily, lisps have a way of generating unlikely names that are guaranteed not
;; to be the same as other unlikely names

(gensym)

;; will make you a new unlikely name every time you call it, which is guaranteed not to be the same as any symbol that can be read in, or the same as any other gensym.

;; So here is the final version of our macro. It's robust, and can now be
;; trusted.  It uses the right println from clojure/core, and it gives its
;; temporary variable a name which cannot collide with any other variable.

(defmacro dbg-4 [s]
  (let [unlikely (gensym)]
    (list 'let [unlikely s]
          (list 'clojure.core/println (list 'quote s) "=" unlikely)
          unlikely)))

;; But it no longer looks much like the code transformation we were originally
;; trying to make.


















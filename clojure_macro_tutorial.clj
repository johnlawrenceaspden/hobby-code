;; A couple of chaps have asked me to write a clojure macro tutorial, to explain
;; my debugging macro

(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

;; Which is used to examine print out intermediate values in functions.

;; Here's an example function that we might want to debug:
(defn pythag [ x y ] (* (* x x) (* y y)))

;; And here is a version enhanced to print out its thought process as it runs
(defn pythag [ x y ]  (dbg (* (dbg (* x x)) (dbg (* y y)))))

(pythag 4 5)
;; (* x x) = 16
;; (* y y) = 25
;; (* (dbg (* x x)) (dbg (* y y))) = 400
;; 400

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I'm going to try to imagine that I didn't know how to write dbg, and had to
;; go at it by trial and error, to show why it is as it is.

;; I think that the best way to learn to do something is by going through lots
;; of examples. I haven't followed this path, so I imagine that no-one is going
;; to learn how to write macros from following this through.

;; But theory helps too. I'd like to try to communicate something of the why and
;; how of macros.

;; You should find lots of examples too. I'll keep an eye out for some simple
;; ones to use in a follow up set of exercises.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The problem:

;; Often the best way to debug code is by adding print statements.

;; Consider our old friend factorial

(defn factorial [n]
  (if (< n 2) n
      (* n (factorial (dec n)))))

(factorial 5) ;; 120

;; How would we watch it at work?
;; This modified version prints out the value of every recursive call:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The problem with this solution is that I've had to do a fair bit of typing to
;; change the function into a version that prints out its intermediate values.

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

;; Notice that the phrase (factorial (dec n)) has to be repeated.

;; Every time I would like to examine the value returned by an expression as my
;; program runs, I have to make this complicated but mechanical change. Even
;; more annoyingly, I have to tell the compiler something that it already knows.

;; Any time you find that you have to do too much typing, you will also find
;; difficulty in reading, and potential for error. It is always to be avoided.

;; This simple repetitive task should be as easy as changing
(factorial (dec n))
;; to
(dbg (factorial (dec n)))

;; Normally, when one spots a common pattern like this, one makes a function.
;; But a function to do what we want here is problematical.

;; We need the source code as well as the evaluated value of (factorial (dec n))
;; Something like:
(defn dbgf [s x]
  (println s "=" x)
  x)

(defn factorial [n]
  (if (< n 2) n
      (dbgf "(* n factorial(dec n))" (* n (factorial (dec n))))))
  
;; That's a bit better, but I'm still changing
(* n (factorial (dec n)))
;; into 
(dbgf "(* n factorial(dec n))" (* n (factorial (dec n))))

;; Which is less error prone, but still repetitive.

;; The reason that we need to hold dbgf's hand like this, telling it what to
;; print out in two different ways, is that a function's arguments are evaluated
;; before the function is called.

;; If we want to write
(dbg (* n (factorial (dec n))))
;; and have it work, then we need to take control of when
;; (* n (factorial (dec n))) is evaluated.

;; And this is the problem that macros solve.

;; We need to work out how to write:
(dbg (* n (factorial (dec n))))
;; And get:
(let [a (factorial (dec n))] (println "(factorial (dec n))=" a) a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now because lisp code and lisp data are very similar things, we can easily
;; write a function which will generate the code that we want:

;; Let's define:
(defn dbg-code [s]
  (list 'let ['a s] (list 'println (list 'quote s) "=" 'a) 'a))
;; Which is just a function that takes a tree of symbols and gives back another
;; tree of symbols.

;; We can call this function on little pieces of code, to get other little
;; pieces of code

(dbg-code 'x)
;; (let [a x] (println (quote x) "=" a) a)

(dbg-code '(* x x))
;; (let [a (* x x)] (println (quote (* x x)) "=" a) a)

(dbg-code '(* n (factorial (dec n))))
;; (let [a (* n (factorial (dec n)))] (println (quote (* n (factorial (dec n)))) = a) a)

;; Nothing 'macro' has gone on yet! This is just a function, taking advantage of
;; lisp's ability to easily deal with the lists and trees and vectors that make
;; up lisp code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now how shall we turn our code-generating function into a macro?

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
;; code, it runs the function, and substitutes the code that is returned into
;; the program.

;; It is like programming the compiler to be an apprentice programmer who will
;; write out the tedious bits longhand for you.

;; We can even ask the compiler what it sees when it expands dbg-1:
(macroexpand-1 '(dbg-1 x))
;; (let [a x] (println (quote x) "=" a) a)
(macroexpand-1 '(dbg-1 (* x x)))
;; (let [a (* x x)] (println (quote (* x x)) "=" a) a)
(macroexpand-1 '(dbg-1 (println x)))
;; (let [a (println x)] (println (quote (println x)) "=" a) a)
(macroexpand-1 '(dbg-1 (inc x)))
;; (let [a (inc x)] (println (quote (inc x)) "=" a) a)
(macroexpand-1 '(dbg-1 (* n (factorial (dec n)))))
;; (let [a (* n (factorial (dec n)))] (println (quote (* n (factorial (dec n)))) "=" a) a)

;; So have we won??

;; We have certainly solved our problem as we originally conceived it, but there
;; are potentially couple of difficulties with our solution.

;; One is that when our macro is expanded, the names of functions and variables
;; may not be what they were when it was defined. You would have the same
;; problem with a careless apprentice! Consider the case when a macro is
;; exported from one namespace into another.

;; For instance, consider the case where we're defining a local version of println.
(defn println [& s]
  (apply clojure.core/println "eek:" s))

(dbg-1 n)
;;eek: n = 5
;;5

;; The problem is that the expanded code calls the new println function,
;; user/println, not the version of println in clojure/core that was around when
;; the macro was defined.

;; Now there will be times when you want this to happen, and times when you do
;; not, and mostly you will not.

;; So a version of the macro which always calls clojure.core/println no matter
;; whether there's a local version or not is:

(defmacro dbg-2 [s]
  (list 'let ['a s] (list 'clojure.core/println (list 'quote s) "=" 'a) 'a))
;; explicitly specifying which namespace we want the function to come from.

(dbg-2 n)
;;n = 5
;;5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What we wanted was a macro that would transform

;; (dbg x)
;; into
;; (let [a x] (clojure.core/println 'x "=" a) a)

;; And we have worked out that the macro that does this is:
(defmacro dbg-2 [s]
  (list 'let ['a s] (list 'clojure.core/println (list 'quote s) "=" 'a) 'a))

;; Now clojure, being a language that is built out of macros, tries to make them
;; as easy as possible to write, and one of the ways in which it helps is the syntax-quote

;; Let's look at the code-generating part of dbg-2
(list 'let ['a s] (list 'clojure.core/println (list 'quote s) "=" 'a) 'a)

;; Notice the number of quote marks! (')

;; In fact this is normal when generating code. We want to do a small number of
;; substitutions (wherever s is), and an awful lot of verbatim copying.

;; We'd like to use the quote operator to make the thing easier to read.

'(let [a s] (println 's "=" a) a)

;; But then we also quote s, and we don't resolve println

;; So clojure provides the syntax-quote ` which is like ', but which allows us
;; to evaluate certain expressions withing the quoted code, and which resolves
;; all symbols. This means that macros can look like the code they're
;; generating.

(def s "hello")
'(let [a s] (println 's "=" a) a) ;;(let [a s] (println (quote s) "=" a) a)
`(let [a s] (println 's "=" a) a) ;; (clojure.core/let [user/a user/s] (clojure.core/println (quote user/s) "=" user/a) user/a)
`(let [a# s] (println ~s "=" a#) a#) ;; (clojure.core/let [a__10842__auto__ user/s] (clojure.core/println "hello" "=" a__10842__auto__) a__10842__auto__)


(defmacro dbg[x]
  `(let [x# ~x] (println '~x "=" x#) x#))




















;; But there is a more subtle problem, to do with our slightly silly choice of
;; temporary variable.

;; If we tried to debug an expression which had an a in it, there would be
;; a certain confusion:
(macroexpand '(dbg-1 (* a a))) ;; (let* [a (* a a)] (println (quote (* a a)) "=" a) a)

;; I can't see a way in which this could cause a problem for dbg-2, it is just
;; bad style. 

;; But in more complicated macros, the interference between the variables
;; introduced by the macros and the variables in the source code can be a source
;; of subtle bugs.

;; Using a temporary variable which could collide with a variable in the real code
;; is an accident waiting to happen, and you shouldn't do it.

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

;; will make you a new unlikely name every time you call it, which is guaranteed
;; not to be the same as any symbol that can be read in, or the same as any
;; other gensym.

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

;; Now that we understand what macros are, and how to avoid the two pitfalls
;; that can break them, in part two we'll look at the various ways in which
;; clojure makes the macro writing process easier, so that you when you want
(dbg x)
;; to become
(let [a x] (println 'x "=" a) a)
;; you can write the more readable:
(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))
;; instead.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Answers to exercises



(defmacro dbg-improved [ & s ]
  (list 'let ['a s] (list 'println (list 'quote s) "=" 'a) 'a))

(dbg-improved * 3 2)


(forloop [i 1 (do (print "hi") 10)]
         (print i)
         (println limit))


(let [limit "hello"]
  (forloop [i 1 (do (print "hi") 10)]
           (print i)
           (println limit)))






(defmacro forloop [[var start finish] & body]
  (list 'loop [ var start ]
    (concat (list 'when (list '<= var finish))
            body
            (list (list 'recur (list 'inc var))))))



(defmacro forloop [[var start finish] & body]
  (list 'let ['limit finish]
    (list 'loop [ var start ]
          (concat (list 'when (list '<= var 'limit))
                  body
                  (list (list 'recur (list 'inc var)))))))



;; hint: use macroexpand-1 to look at the code for (dbg (print "yo"))












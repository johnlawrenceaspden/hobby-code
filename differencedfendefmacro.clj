;; The question "What is the difference between defn and defmacro" came up on
;; Stack Overflow

;; http://stackoverflow.com/questions/3667403/what-is-the-difference-between-the-defn-and-defmacro/3668091#3668091

;; Here is my answer, which combines my little debugging macro (which I miss in
;; every non-lisp programming language that I use) with 'the story of mac'.

;; I worry that I might have over-simplified when I was trying to be clear.
;; Remember that the question is being asked by someone who does not already
;; understand the answer.

;; Have I misspoken or misled here?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A macro is like having an apprentice programmer that you can write notes to:

;; Sometimes, if I'm trying to debug something, I like to change something like

    (* 3 2)

;; Into something like this:

    (let [a (* 3 2)]
      (println "dbg: (* 3 2) = " a)
      a)

;; Which works the same way, except that it prints out the expression it has
;; just evaluated, and its value, as well as returning the value as the result
;; of the whole expression. This means that I can leave my code undisturbed
;; whilst examining intermediate values.

;; This can be very useful, but it's time consuming and error prone to type. You
;; might imagine delegating such tasks to your apprentice!

;; One way to do that would be to write (dbg (* 3 2)) and leave to your
;; apprentice the mechanical task of turning that into the above code.

;; Rather than hiring an apprentice, you can program the compiler to do these
;; things for you:

    ;;debugging parts of expressions
    (defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;; Now try:

    (* 4 (dbg (* 3 2)))

;; The compiler actually makes the textual transformation on the code for you,
;; although being a computer, it chooses unreadable names for its variables
;; instead of the "a" I would have chosen.

;; We can ask it what it would do for a given expression:

    (macroexpand '(dbg (* 3 2)))

;; And this is its answer, so you can see that it really is rewriting the code
;; for you:

    (let* [x__1698__auto__ (* 3 2)]
          (clojure.core/println "dbg:" (quote (* 3 2)) "=" x__1698__auto__)
          x__1698__auto__)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Try to write a function dbgf that does the same thing, and you'll have
;; problems, because (dbgf (* 3 2)) -> (dbgf 6) before dbgf is called, and so
;; whatever dbgf does, it can't recover the expression that it needs to print
;; out.

;; I'm sure you can think of many ways round this, like run-time evaluation or
;; passing in a string. Try to write dbg using defn instead of defmacro. It will
;; be a good way to convince yourself that macros are good things to have in a
;; language. Once you've got it working, try using it on an expression that has
;; a side effect as well as a value, like:

    (dbg (print "hi"))

;; (In fact macros are so good to have that we're prepared to live with the
;; (brackety ((syntax))) of LISPs in order to get them. (Although I must say
;; that I rather like it for its own sake (but then (I am) a bit weird (in the
;; head.))))

;; C also has macros, which work in roughly the same way, but they're always
;; going wrong, and to get them right you need to put so many brackets into your
;; program that it looks like LISP!

;; You're actually recommended not to use C's macros because they're so error
;; prone, although I have seen them used to great effect by people who really
;; knew what they were doing.

;; LISP macros are so effective that the language itself is built out of them,
;; as you'll notice if you look at the Clojure source files that are themselves
;; written in Clojure.

;; The base language is very simple so that it's easy to implement, and then the
;; complex superstructure is built up using macros.










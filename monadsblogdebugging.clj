;;A monad for debugging:


;;This is part of a series I'm writing in order to help me understand monads. 
;;If you can't follow it, read my earlier posts:

;;http://learnclojure.blogspot.com/2009/09/how-it-works-monad-im-currently-reading.html
;;http://learnclojure.blogspot.com/2009/10/sequence-monad.html

;;You might also like:
;;http://onclojure.com/2009/03/05/a-monad-tutorial-for-clojure-programmers-part-1/
;;The first two parts are just paraphrases of material in this excellent tutorial.

;;Often, when trying to understand the behaviour of a program, we use print statements. 
;;One adds the statements to functions, and as the program runs, output goes to the screen.

;;One might wish to capture such output programatically. 
;;One way is to rebind the print function, another is to use a special logging library.

;;Another way is to create a string building object, and thread it through every function which
;;is called, adding information to it at every step.

;;I have done all these things at one time or another.

;;A less side-effect dependent way to do it would be to get the functions themselves to return, 
;;as well as their values, strings describing what they have done.

;;Here are a couple of 'debuggable functions':
(defn d+ [a b] [(+ a b), (format "(+ %s %s)->%s;" a b (+ a b))])
(defn d* [a b] [(* a b), (format "(* %s %s)->%s;" a b (* a b))])

;;try
(d+ 1 2)
(d* 2 3)

;;In fact, we can make a macro which turns arbitrary functions into debuggable functions:
(defmacro dbg-fn [function]
  `(fn [& vals# ] 
     (let [result# (apply ~function vals#)
           rs# (if (nil? result#) "nil" (str result#))]
           [result# (str (cons '~function vals#) "->" rs# ";")])))

(def d+ (dbg-fn +))
(def d- (dbg-fn -))
(def d* (dbg-fn *))
(def dlist (dbg-fn list))
(d+ 1 2 3)
(dlist 1 2 3)
(d* 1 2 3 4)

;;This approach looks promising, but we've lost the fundamental ability to combine our
;;debuggable functions. 

;; (d+ ( d+ 1 2) 3) is an error

;;Debuggable functions return a container holding a value and a string.
;;In order to pass this to another debuggable function, we need to 
;;unpack the container, take out the value, and pass it into the second function.

;;If we save the first string somewhere, and then add the string from the output of the
;;second function, and then pack up the two strings and the new value into a new container,
;;then we will have a way to compose our debuggable functions to get new debuggable functions.

;;By certain definitions, we will have defined a new programming language!

;;When a function is able to tell its calling program what it has done in order to calculate
;;the value it returns, we have, as well as a debugging method, a way of examining algorithms, 
;;a type of compiler, an opportunity to move run-time calculations back to compile time, to
;;specialize algorithms to particular arguments. This may or may not turn out to be useful,
;;but it is surely interesting.

;;But it is obvious that all this packing and unpacking will get very old very quickly if we try 
;;to do it manually, and that's why this functional debugging style has never really caught on....

;;I once tried it, and threw up my hands in despair after a day or so of pain.

;;........

;;Unfortunately, 
'(d+ ( d+ 1 2) 3) 
;;is an error
;;So is the equivalent: 
'(let [a (d+ 1 2)
       b (d+ a 3)]
   b)

;;This looks more promising, but it's fiddly:
(let [[a as] (d+ 1 2)
      [b bs] (d+ a 3)]
  [b (str as bs)])
;;We are like the man trying to solve a problem using regular expressions.
;;Now we have two problems.


;;Monads can help. 
;;Chaining of functions is what let is about.
;;Chaining of functions with incompatible types is what monads are about

;;Here's our do-monad macro from before:
(defmacro do-monad [[binder result] bindings expression]
  (if (= 0 (count bindings))
    `(~result ~expression)
    `(~binder ~(second bindings) (fn[~(first bindings)]
       (do-monad [~binder ~result] ~(drop 2 bindings) ~expression)))))

;;What we'd like to write is something like:
'(debug-let [a (d+ 1 2)
             b (d+ a 3)
             c (d+ a b)]
            c)

;;We want to define dbg-bind and dbg-result so that we can use:
'(do-monad [dbg-bind dbg-result]
           [a (d+ 1 2)
            b (d+ a 3)
            c (d+ a b)]
           c)

;;We know that this macro will expand into:
'(dbg-bind (d+ 1 2) (fn [a] 
    (dbg-bind (d+ a 3) (fn [b] 
       (dbg-bind (d+ a b) (fn [c] 
         (dbg-result c)))))))


;;dbg-result obviously has to turn a value into a [value string] pair.
;;So we might try:
(defn dbg-result [value]
  [value ""])

;;dbg-bind must take:
;; a [value string] pair, and 
;; a function which takes a value and returns a [value string] pair.

;;and it must return a pair made out of the final value and the concatenation of the two strings.
;;Once we've stated the problem, it can really only be:
(defn dbg-bind [[value string] function]
  (let [[r s] (function value)]
    [r (str string s)]))

;;Here's an example:
(do-monad [dbg-bind dbg-result]
                         [a (d+ 1 2)
                          b (d+ a 3)
                          c (d+ a b)
                          d (d* b c)
                          e (dlist a b c d)
                          f ((dbg-fn print) a b c d e)]
                         (list a b c d f))

;;Notice how the printing we did in the middle of the computation hasn't got mixed up
;;with the debug printing, like it might have done using some approaches.

(do-monad [dbg-bind dbg-result]
                         [x (dbg-result 1)
                          y (dbg-result 2)
                          z (dbg-result 3)
                          a (d+ x y)
                          b (d+ a z)
                          c (d+ a b)
                          d (d* b c)
                          e ((dbg-fn /) d c)]
                         e)


;;Our means of combination are still general:

(defn debug-fib [n]
  (if (< n 2) [n (str "(fib " n ")->" n ";")]
      (do-monad [dbg-bind dbg-result]
                [f1 (debug-fib (- n 1))
                 f2 (debug-fib (- n 2))
                 f3 (d+ f1 f2)]
                f3)))

(debug-fib 4)

;;It is interesting to note that we have created a new and different sort of thing here:
;;To see how different, let us combine our monadic debugging method with the more traditional
;;side-effect based debugging approach

(defn debug-fib [n]
  (do (print "fib " n)
      (if (< n 2) [n (str "(fib " n ")->" n ";")]
      (do-monad [dbg-bind dbg-result]
                [f1 (debug-fib (- n 1))
                 f2 (debug-fib (- n 2))
                 f3 (d+ f1 f2)]
                f3))))

;;And let's memoize the function
(def debug-fib (memoize debug-fib))

(debug-fib 5)

;;What's the ratio of 'operations we're interested in' to the value of the fib function?

(def debug-fibs (map debug-fib (iterate inc 1)))

;;Here's the fibonnacci sequence
(def fib-vals (map first debug-fibs))
(def fib-traces (map #(clojure.contrib.str-utils/re-split #";" %) (map second debug-fibs)))
(def fib-complexities (map count fib-traces))

;;And here are the first few ratios
(take 6 (map #( / %1 %2 1.0) fib-complexities fib-vals))




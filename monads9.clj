;;A Simple Monad Example

;;There's an excellent tutorial on monads here:
 
;;http://onclojure.com/2009/03/05/a-monad-tutorial-for-clojure-programmers-part-1/

;;Here's a very simple example of the use of monads, 
;;which are nothing more scary than a generalization of 'let'.

;;We'll calculate Pythagoras' rule in words.

;; Here are some numbers as strings:
(def integers (partition 2 '[1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 12 "twelve"]))
;; This is a little recursion to look up a string or number and return its partner:
(defn swap
  ([x] (swap x integers))
  ([x lst]
     (if (=(count lst) 0) nil
         (let [[a s] (first lst)]
           (if (= x a) s
               (if (= x s) a
                   (recur x (rest lst))))))))

;; The function returns nil if it can't find a match. This seems reasonable behaviour
;; for our pythagoras function too.

(defn buggy-pythag[xs ys]
  (let [x (swap xs)
        y (swap ys)
        x2 (* x x)
        y2 (* y y)
        z (Math/sqrt (+ x2 y2))
        zs (swap z)]
    zs))

;; Now 
(buggy-pythag "three" "four") 
;;is 
"five" 
;; as we expect, but sadly, if we call 
(buggy-pythag "one" "six")
;;, then an exception is thrown.

;; The problem is that nils passed into the maths functions cause exceptions.
;; In order to make the program work we'd have to wrap all the calls to swap
;; in code to detect nils and return nil instead of proceeding with the calculation.
 
;; Fortunately, we can replace the let statement with a monadic computation which does just that.
;; There's a built-in monad library:
(use 'clojure.contrib.monads)

;; The identity monad is an exact drop-in replacement for let.
;; The maybe monad is like that, but also does what we want, 
;; immediately returning nil as the result of the whole computation as 
;; soon as it sees one produced as the value of an intermediate step.
;; All we have to do is replace 'let' with 'domonad maybe-m'.

(defn pythagoras [xs ys]
  (domonad maybe-m
           [x (swap xs)
            y (swap ys)
            x2 (* x x)
            y2 (* y y)
            z (Math/sqrt (+ x2 y2))
            zs (swap z)]
           zs))

;;Now we can call our function safely on any two strings:
(pythagoras "three" "four")
(pythagoras "four" "five")
(pythagoras "one" "six")
(pythagoras "five" "twelve")

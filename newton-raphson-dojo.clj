;; When we write lisp, we have magic powers, and the reason is that our code is in a form that is easy to manipulate
;; programmatically.

;; Like all magic, there is a price to pay.

;; The price is that we have to rip the front end off our compiler.

;; The front end is the thing that takes 1*2+3*5 and says

;; 1 is valid but 1* isn't , and so on, so that must really be 1 * 2 + 3 * 5 
;; * binds tighter than +, so that's really ( ( 1 * 2 ) + ( 3 * 5 ) )
;; Which is really a tree:

             (+ 
              (* 5 3)
              (* 1 2))

;; Meaning take 1 and 2 and multiply them, take 5 and 3 and multiply them. Add the results.

;; This is clearly seriously annoying, and like monads in Haskell, it's the first thing you hit when you learn lisp, 
;; but I promise that after a month or so of using it you stop noticing it, 
;; and although it's never quite as good for actual arithmetic, it's actually much nicer as a notation for a 
;; generalized function call.

;; More importantly, it allows us to treat all functions uniformly, including the ones we define 
;; ourselves. And this is the source of the magic.

;; If we do well today we'll be able to symbolically differentiate a function. Using a very short program. 
;; And that's hard in a language with a syntax.

;; So my aim for today is to get us used to the (lack of) syntax of lisp. 

;; We're going to use clojure, which is a modern member of the LISP family, 
;; in the same sense that Java is a member of the ALGOL family.

;; Clojure's a wonderful language, which uses Java bytecode as its machine language, and runs on the JVM, 
;; and so has easy natural access to all the libraries that Java has. 

;; My friends at the Chemistry department in Cambridge are using it because they've written a lot of Java over the years, 
;; and they like it, but they've found that Clojure is an easier way to write Java than Java is.

;; It has pervasive laziness throughout the language, which allows us to 
;; disconnect the things that need to be done from the order they need to be done in.  
;; It has extraordinary pure-functional data structures, and baked-in software transactional memory, 
;; which together make for a style of programming that can run in parallel on many cores.

;; But I'm going to ignore all of that, and we're going to get over the pons asinorum of lisp, 
;; which is all those damned brackets.

;; We're going to write a program that you could have written in lisp forty years ago, when the newest member 
;; of the ALGOL family was ALGOL itself.


;; -------------------------------------------------------------------------------------------------

;; The method of Heron of Alexandria
;; ---------------------------------

;; I don't know if any of you remember not having a calculator. 
;; Once upon a time they didn't even have mathematical tables, and so they had to calculate things like square roots by hand.

;; One popular method of calculating a square root is due to Hero(n) of Alexandria, 
;; who also invented steam power, the windmill, the syringe and the vending machine.

;; What Heron said to do was this:

;; Suppose you have a number that you want to find the square root of, let's say 10.

;; And suppose you have a guess for where that square root might be, let's say 1. 
;; It's not a very good guess, because the square of 1 is 1, not ten, but it will do to get started.

;; So Heron tells us, if we know the square root, then if we divide 10 by it, we'll get it back.
;; Like say the square root of 4 is 2, and if we divide 4 by 2, then we get 2. 
;; And that's what it means to *be* a square root.

;; But if we guessed too low, then the thing we get back will be too high.
;; And if it's too high, then the thing that we get back will be too low.

;; So Heron says to take the average of what we have and what we get back when we do this division, 
;; and he promises us that will be a better guess.

;; Let's try that, for our problem number 10, and our guess 1
user> (/ 10 1)
10

;; We guessed 1, we divided 10 by 1, we got back 10.
;; What's the average?
user> (/ (+ 10 1) 2)
11/2 

;; Or if we do the whole calculation at once, it looks like:
user> (/ (+ (/ 10 1) 1) 2)
11/2

;; That's getting a bit hard to read, so we should define a function to give us averages
user> (defn average [a b] (/ (+ a b) 2))
#'user/average

;; We can test it
user> (average 10 1)
11/2

;; So our old calculation goes like
user> (average (/ 10 1) 1)
11/2

;; We might as well make another function which just makes our guesses better.
user> (defn improve-guess [guess] (average guess (/ 10 guess)))
#'user/improve-guess

;; Let's try that
user> (improve-guess 1)
11/2

;; Of course, a better guess can also be improved
user> (improve-guess (improve-guess 1))
161/44

;; And improved again
user> (improve-guess (improve-guess (improve-guess 1)))
45281/14168

;; Now you'll notice that clojure is doing exact arithmetic, and giving us back a fraction, just like a human being would do. 
;; But if we start it off with an inexact guess, a decimal rather than a rato, say 1.0 rather than 1
;; Then because you can't make an exact answer from an inexact input, every answer we get back will be a decimal.
user> (improve-guess (improve-guess (improve-guess 1.0)))
3.196005081874647

;; inexactness is contagious.
user> (+ 1 2)
3
user> (+ 1.0 2)
3.0

;; Let's go on a bit of a side-track now and look at just how good Heron's great idea is. We can use a magic function called 
;; iterate to make an infinite sequence of guesses. It's best only to look at the first bit of an infinite sequence. 
;; Otherwise, like with the Medusa of Greek myth, the REPL turns to stone. Let's look at the first five values in our
;; sequence.
user> (take 5 (iterate improve-guess 1))
(1 11/2 161/44 45281/14168 4057691201/1283082416)

;; We get a better idea of what's going on here if we use decimal fractions
user> (take 5 (iterate improve-guess 1.))
(1.0 5.5 3.659090909090909 3.196005081874647 3.16245562280389)

;; It only takes a few iterations before we've hit the limit of floating-point accuracy
user> (take 10 (iterate improve-guess 1.))
(1.0 5.5 3.659090909090909 3.196005081874647 3.16245562280389 3.162277665175675 3.162277660168379 3.162277660168379 3.162277660168379 3.162277660168379)

;; Let's look at the squares of the values in our sequence:
user> (map (fn[x](* x x)) (iterate improve-guess 1.))
;; Oops. We looked at the medusa! In some environments the REPL is smart enough to print out the values one-by-one, 
;; but here, it's trying to do the whole thing before printing anything.
;; Ctrl-C will stop the calculation and restore the REPL.

; Evaluation aborted.

;; Try again:
user> (take 10 (map (fn[x](* x x)) (iterate improve-guess 1.)))
(1.0 30.25 13.388946280991735 10.21444848336857 10.001125566203939 10.000000031668918 9.999999999999998 9.999999999999998 9.999999999999998 9.999999999999998)

;; But iterate and map are magic functions, so let's not worry too much about lazy sequences just yet, and see how we might do it the hard way.

;; First off, we'd like a definition of when our guess is good enough.
;; How about, square it and if the answer's close to 10 then that goes?
;; How good a guess is 3.19?
user> (- 10 (* 3.19 3.19))
-0.17609999999999992

;; Rats, we need an absolute value function
user> (defn abs[x] (if (< x 0) -x x))
; Barf....

; Sigh. Every function needs the same call syntax. Unary minus is a function just like anything else.
user> (defn abs[x] (if (< x 0) (- x) x))
#'user/abs

; Let's use map to test the function.
user> (map abs (list -1 1 0))
(1 1 0)

;; So:
user> (abs (- 10 (* 3.19 3.19)))
0.17609999999999992

;; What's good enough? Say if the difference is less than 1/10^6, or 1e-6
user> 1e-6
1.0E-6

;; Here's our good-enough test
user> (< (abs (- 10 (* 3.19 3.19))) 1e-6)
false
;; 3.19 isn't good enough

;; Wrap the test up
user> (defn good-enough? [guess] (< (abs (- 10 (* guess guess))) 1e-6))
#'user/good-enough?

;; Let's see whether the first five values are good enough
user> (take 5 (map good-enough? (iterate improve-guess 1.)))
(false false false false false)

;; What about the first ten?
user> (take 10 (map good-enough? (iterate improve-guess 1.)))
(false false false false false true true true true true)

;; What were those answers again?
user> (take 10 (iterate improve-guess 1.))
(1.0 5.5 3.659090909090909 3.196005081874647 3.16245562280389 3.162277665175675 3.162277660168379 3.162277660168379 3.162277660168379 3.162277660168379)

;; So what if we just want a function that will give us an answer that is good enough?
;; We'll call it good-enough-guess. We give it a guess. If that guess is good enough, then it gives us it back.
;; If it's not, then it makes it better, and tries again.
user> (defn good-enough-guess [x]
        (if (good-enough? x) x
            (good-enough-guess (improve-guess x))))
#'user/good-enough-guess

;; It doesn't really matter what our initial guess is. Anything will work.
user> (good-enough-guess 1.0)
3.162277665175675

user> (good-enough-guess 3.0)
3.162277660169842

;; How good is that guess?
user> (* (good-enough-guess 3.0)(good-enough-guess 3.0))
10.00000000000925

;; Sweet. Here endeth the method of Heron of Alexandria for finding the square root of 10.

;; Let's have a look at our program in its entirety

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn average [a b] 
  (/ (+ a b) 2))

(defn improve-guess [guess]
  (average guess (/ 10 guess)))

(defn abs[x] 
  (if (< x 0) (- x) x))

(defn good-enough? [guess] 
  (< (abs (- 10 (* guess guess))) 1e-6))

(defn good-enough-guess [x]
  (if (good-enough? x) x
      (good-enough-guess (improve-guess x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



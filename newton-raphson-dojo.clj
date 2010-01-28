;; I don't know if any of you remember not having a calculator. 
;; Once upon a time they didn't even have mathematical tables, and so they had to calculate things like square roots by hand.

;; One popular method of calculating square root is that due to Hero(n) of Alexandria, 
;; who also invented steam power, the windmill, the syringe and the vending machine.

;; What Heron said to do was this:

;; Suppose you have a number that you want to find the square root of, let's say 10.

;; And suppose you have a guess for where that square root might be, let's say 1. 
;; It's not a very good guess, because the square of 1 is 1, not ten, but it will do.

;; So Heron tells us, if we know the square root, then if we divide 10 by it, we'll get it back.

;; But if we guessed too low, then the thing we get back will be too high.
;; And if it's too high, then the thing that we get back will be too low.

;; So Heron says to take the average of what we have and what we get back, and that will be a better guess.

;; Let's try that
user> (/ 10 1)
10
user> ;; We guessed 1, we divided 10 by 1, we got back 10.
;; What's the average?
user> (/ 2 (+ 10 1))
2/11
user> (/ (+ 10 1) 2)
11/2 
user> ;; Or if we do the whole calculation at once, it looks like:

user> 

user> (/ (+ (/ 10 1) 1) 2)
11/2
user> ;; Already that's getting a bit hard to read, so now we want to make a function to give us averages

nil
user> (defn average [a b] (/ (+ a b) 2))
#'user/average
user> ;; We can test it
nil
user> (average 10 1)
11/2
user> ;; So our old calculation goes like
nil
user> (average (/ 10 1) 1)
11/2
user> ;; We might as well make another function which just makes our guesses better.
nil
user> (defn improve-guess [guess]
        (average guess (/ 10 guess)))
#'user/improve-guess
user> (improve-guess 1)
11/2
user> (improve-guess (improve-guess 1))
161/44
user> (improve-guess (improve-guess (improve-guess 1)))
45281/14168
user> ;; Now you'll notice that clojure is doing exact arithmetic, and giving us back a fraction, just like a human being would do. 
nil
user> ;; But if we start it off with an inexact guess, a decimal rather than a rato, say 1.0 rather than 1
nil
user> (improve-guess (improve-guess (improve-guess 1.0)))
3.196005081874647
user> ;; Then because you can't make an exact answer from an inexact input, every answer we get back will be a decimal.
nil
user> (+ 1 2)
3
user> (+ 1.0 2)
3.0
user> ;; inexactness is contagious.

;; Let's go on a bit of a side-track now and look at just how good Heron's great idea is. We can use iterate to 
user> make an infinite sequence of guesses. It's best only to look at the first bit of an infinite sequence. Otherwise the REPL turns to stone.
user> (take 5 (iterate improve-guess 1))
(1 11/2 161/44 45281/14168 4057691201/1283082416)
user> (take 5 (iterate improve-guess 1.))
(1.0 5.5 3.659090909090909 3.196005081874647 3.16245562280389)
user> (take 10 (iterate improve-guess 1.))
(1.0 5.5 3.659090909090909 3.196005081874647 3.16245562280389 3.162277665175675 3.162277660168379 3.162277660168379 3.162277660168379 3.162277660168379)
user> (map (fn[x](* x x)) (iterate improve-guess 1.))
; Evaluation aborted.
user> (take 10 (map (fn[x](* x x)) (iterate improve-guess 1.)))
(1.0 30.25 13.388946280991735 10.21444848336857 10.001125566203939 10.000000031668918 9.999999999999998 9.999999999999998 9.999999999999998 9.999999999999998)
user> ;; But iterate is a magic function, so let's not worry too much about lazy sequences just yet, and see how we might do it the hard way.
nil
user> ;; First off, we'd like a definition of when our guess is good enough.
nil
user> ;; How about, square it and if it's close to 10 then that goes?
nil
user> (- 10 (* 3.19 3.19))
-0.17609999999999992
user> ;; Rats, we need an absolute value function
nil
user> (defn abs[x] (if (< x 0) -x x))
; Sigh. Every function needs the same call syntax. Unary minus is a function.
user> (defn abs[x] (if (< x 0) (- x) x))
#'user/abs
user> (defn abs[x] (if (< x 0) (-x) x))
; Evaluation aborted.
user> (defn abs[x] (if (< x 0) (- x) x))
#'user/abs
user> (map abs (list -1 1 0))
(1 1 0)
user> ;; So:
nil
user> (abs (- 10 (* 3.19 3.19)))
0.17609999999999992
user> ;; What's good enough? Say if the difference is less than 1/10^6
nil
user> 1e-6
1.0E-6
user> (< (abs (- 10 (* 3.19 3.19))) 1e-6)
false
user> ;; Wrap that up
nil
user> (defn good-enough? [guess] (< abs (- 10 (* guess guess)) 1e-6))
#'user/good-enough?
user> (map good-enough (iterate improve-guess 1.))
; Evaluation aborted.
user> (take 5 (map good-enough? (iterate improve-guess 1.)))
; Evaluation aborted.
user> (defn good-enough? [guess] (< (abs (- 10 (* guess guess))) 1e-6))
#'user/good-enough?
user> (take 5 (map good-enough? (iterate improve-guess 1.)))
(false false false false false)
user> (take 10 (map good-enough? (iterate improve-guess 1.)))
(false false false false false true true true true true)
user> (take 10 (iterate improve-guess 1.))
(1.0 5.5 3.659090909090909 3.196005081874647 3.16245562280389 3.162277665175675 3.162277660168379 3.162277660168379 3.162277660168379 3.162277660168379)
user> ;; So what if we just want a function that will give us an answer that is good enough?

nil
user> nil
user> ;; We'll call it good-enough-guess. We give it a guess. If that guess is good enough, then it gives us it back.
nil
user> ;; If it's not, then it makes it better, and tries again.
nil
user> (defn good-enough-guess [x]
        (if (good-enough? x) x
            (good-enough-guess (improve-guess x))))
#'user/good-enough-guess
user> (good-enough-guess 1.0)
3.162277665175675
user> (good-enough-guess 3.0)
3.162277660169842
user> (* (good-enough-guess 3.0)(good-enough-guess 3.0))
10.00000000000925
user> ;; Sweet. Here endeth the method of Heron of Alexandria for finding the square root of 10.

nil
user> nil
user> ;; Let's have a look at our program:
nil
user> (defn good-enough-guess [x]
        (if (good-enough? x) x
            (good-enough-guess (improve-guess x))))
#'user/good-enough-guess
user> (defn good-enough? [guess] (< (abs (- 10 (* guess guess))) 1e-6))
#'user/good-enough?
user> (defn improve-guess [guess]
        (average guess (/ 10 guess)))
#'user/improve-guess
user> (defn abs[x] (if (< x 0) (- x) x))
#'user/abs
user> (defn average [a b] (/ (+ a b) 2))
#'user/average
user> ;;Now let's tidy that up and put it into a file.
; Evaluation aborted.
user> 
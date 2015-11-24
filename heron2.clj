;; We've so far implemented Heron's method for finding the square root of
;; the number 10. The program looks like this:

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

;; At this point, it might be a good idea to put the functions in a file. 
;; If we use a lisp-conscious IDE (EMACS, the Editor for Middle Aged Computer Scientists, is my favourite)
;; Then we can carry on our conversational style of development.

;; In fact, this program is easily generalized. Our first task is to make a more general
;; iterative-improve function.  This function should take a guess, a function to improve guesses,
;; and a function to tell when guesses are good enough, and return an answer which is good enough,
;; which it should find by repeatedly improving the guesses.

;; We almost have this function already. We just need to make the constants in good-enough-guess into arguments.

(defn iterative-improve [x improve good?]
  (if (good? x) x
      (iterative-improve (improve x) improve good?)))

;; Let's test that: (In emacs, put the cursor at the end of the expression and type C-x C-e to evaluate the expression)

(iterative-improve 1.0 improve-guess good-enough?)

;; Or in fact, we can use C-u C-x C-e to evaluate it and paste it into the buffer.

(iterative-improve 1.0 improve-guess good-enough?)
3.162277665175675

;; (* 3.162277665175675 3.162277665175675) is 10.000000031668918, so we haven't broken our method.

;; Of course to find the square roots of numbers other than 10, we need functions that will make our guesses closer and tell whether the answers are good enough.

;; It would be a terrible thing to have to hand code them every time. But we can make functions which make functions:

(defn make-improver [n]
  (fn [guess] (average guess (/ n guess))))

(def f (make-improver 25))

(f 1.0)                                 ;13.0
(f (f 1.0))                             ;7.461538461538462
(take 10 (iterate f 1.0))               ;(1.0 13.0 7.461538461538462 5.406026962727994 5.015247601944898 5.000023178253949 5.000000000053722 5.0 5.0 5.0)

(defn make-good-enough? [n]
  (fn [guess] (< (abs (- n (* guess guess))) 1e-6)))

(def g? (make-good-enough? 25))

(take 10 (map g? (iterate f 1.0)))      ;(false false false false false false true true true true)

(iterative-improve 1.0 f g?)            ;5.000000000053722

;; So now we can find arbitrary square roots

(defn square-root [n]
     (iterative-improve 1.0 (make-improver n) (make-good-enough? n)))

(square-root 2)                         ;1.4142135623746899

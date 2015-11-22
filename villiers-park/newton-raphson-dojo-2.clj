user> 
user> ;; So we've written a little program to find the square root of 10 using Heron's method.

;; Well, this is a sweet little program, but it isn't doing anything that you couldn't do with C, 
;; And in fact, if you were to use the common design pattern of 'function call', you could very easily 
;; have written almost exactly the same program in assembler.

;; One notable difference from the C version is that we can do exact arithmetic if we like
user> (good-enough-guess 1)
32927862745156792961/10412704459122043232

user> (defn average [a b]  
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

#'user/good-enough-guess
user> (good-enough-guess 1.0)
3.162277665175675
user> 
nil
user> ;; The next thing is to extend it to find other square roots
nil
user> ;; We need to replace the 10s in improve-guess and good-enough.
nil
user> ;; We don't want to have to edit the code every time, so we'll make functions that make the functions we need
nil
user> (defn make-improve-guess [n]
        (fn [guess] (average guess (/ n guess))))
#'user/make-improve-guess
user> (defn make-good-enough [n]
        (fn [guess] (< (abs (- n (* guess guess))) 1e-6)))
#'user/make-good-enough
user> ;; And we'll change good-enough-guess to take these functions as parameters
nil
user> (defn good-enough-guess [x improve-guess good-enough?]
  (if (good-enough? x) x
      (good-enough-guess (improve-guess x) improve-guess good-enough?)))
#'user/good-enough-guess
user> ;; Remembering that we need to pass these along when good-enough-guess calls itself!
nil
user> ;; Let's try
nil
user> (good-enough-guess 1.0 (make-improve-guess 16) (make-good-enough 16))
; Evaluation aborted.
user> ((make-improve-guess 16) 1)
11/2
user> ((make-improve-guess 16) ((make-improve-guess 16) 1))
161/44
user> ((make-good-enough 16) 4)
true
user> ((make-good-enough 16) 4.1)
false
user> ((make-good-enough 16) 4.01)
false
user> ((make-good-enough 16) 4.001)
false
user> ((make-good-enough 16) 4.0001)
false
user> ((make-good-enough 16) 4.00001)
false
user> ((make-good-enough 16) 4.000001)
false
user> ((make-good-enough 16) 4.0000001)
true
user> (def mig (make-improve-guess 16))
#'user/mig
user> (mig 1.0)
5.5
user> (mig (mig 1.0))
3.659090909090909
user> (mig (mig (mig 1.0)))
3.196005081874647
user> (mig (mig (mig (mig 1.0))))
3.16245562280389
user> (good-enough-guess 1.0 (make-improve-guess 16) (make-good-enough 16))
4.000000000000051
user> ;; Now let's wrap that up
nil
user> (defn sqrt [n] (good-enough-guess 1.0 (make-improve-guess n) (make-good-enough n)))
#'user/sqrt
user> (sqrt 16)
4.000000000000051
user> (map sqrt (range 30))
(9.765625E-4 1.0 1.4142135623746899 1.7320508100147274 2.0000000929222947 2.236067977499978 2.4494897427875517 2.6457513111113693 2.8284271250498643 3.000000001396984 3.162277665175675 3.3166248052315686 3.4641016533502986 3.6055513629176015 3.7416573867739458 3.872983346207433 4.000000000000051 4.123105625617805 4.2426406871196605 4.358898943541577 4.47213595500161 4.582575694960138 4.690415759832057 4.795831523329245 4.898979485596715 5.000000000053722 5.099019513684702 5.19615242285917 5.291502622375404 5.385164807522041)
user> 
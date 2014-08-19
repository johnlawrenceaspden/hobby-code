;; When I say

(def a (* 4 5)) ;-> #'user/a

;; I'd rather the repl told me that I'd just assigned the value 20 to something rather than that I'd just assigned something to user/a

a ;-> 20

;; A macro for this is easy

(defmacro define [var expr]
  `(let [tmp# ~expr]
     (def ~var tmp#) tmp#))

(define a (* 4 5)) ;-> 20

;; I'd also like to be able to say, in a scheme-like manner

(define (square x) (* x x))

;; meaning
(defn square [x] (* x x)) ; #'user/square

;; So I can modify that macro:

(defmacro define [var expr]
  (cond (symbol? var)  `(let [tmp# ~expr] (def ~var tmp#) tmp#)
        (list? var)  `(defn ~(first var) ~(into [] (rest var)) ~expr)))

(macroexpand '(define a (* 20 20))) ;-> (let* [tmp__1986__auto__ (* 20 20)] (def a tmp__1986__auto__) tmp__1986__auto__)
(macroexpand '(define (square x) (* x x))) ;-> (def square (clojure.core/fn ([x] (* x x))))

(define a (* 20 20)) ; 400
(define (cube x) (* x x x)) ;-> #'user/cube
(cube 20) ;-> 8000

;; I must say, I haven't actually tried this in practice yet, but it looks like it might work

(define (random-error)
  (+ (rand) -0.5)) ;-> #'user/random-error

(random-error) ;-> -0.28442993770155234
(random-error) ;-> 0.2911519817783499
(random-error) ;-> -0.4037254523155406

(define bell (/ (reduce + (repeatedly 10 random-error)) 10)) ;-> 0.015416035491431623

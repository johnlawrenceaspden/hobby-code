;; Here we define a clojure factorial function, with clojure-style tests
;; and also export it as a java class so that it can be called from java

;; Here is the boilerplate to export the clojure factorial function
;; An app object has a factorial method
(ns com.aspden.app
  (:gen-class
   :methods [
             [factorial [int] int]
             ])
  (:use clojure.test))

(declare factorial) ;; have to hold the compiler's hand a bit here

;this function is attached to app.factorial, and calls the clojure function
(defn -factorial [this n] (factorial n))



;;defining a clojure version of factorial, with some associated tests

(with-test

    (defn factorial [n]
      (if (< n 3) n
          (* n (factorial (dec n)))))

  (is ( = (factorial 1)     1) "base case")
  (is ( = (factorial 4)     (* 1 2 3 4)) "first recursion")
  ;deliberate fail
  (is ( = (factorial 10)    (apply * (range 1 10))) "general case" ))



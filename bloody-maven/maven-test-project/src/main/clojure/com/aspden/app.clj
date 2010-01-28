;;We're simultaneously defining a clojure namespace com.aspden.app, and a java class com.aspden.app
;;An app object has a factorial method, which can be called from java
(ns com.aspden.app
  (:gen-class
   :methods [
             [factorial [int] int]
             ])
  (:use clojure.test))

;;Defining a clojure version of factorial, with some associated tests
(with-test

    (defn factorial [n]
      (if (< n 3) n
          (* n (factorial (dec n)))))

  (is ( = (factorial 1)     1) "base case")
  (is ( = (factorial 4)     (* 1 2 3 4)) "first recursion")
  ;deliberate fail
  (is ( = (factorial 10)    (apply * (range 1 10))) "general case" ))

;this function is attached to app.factorial, and calls the clojure function
(defn -factorial [this n]
  (factorial n))


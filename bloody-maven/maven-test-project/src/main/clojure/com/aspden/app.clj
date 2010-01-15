(ns com.aspden.app
    (:gen-class
    :state state
    :init init
    :methods [
    [factorial [int] int]
    ])
  (:use clojure.test))

(with-test

    (defn factorial [n]
      (if (< n 3) n
          (* n (factorial (dec n)))))

  (is ( = (factorial 1)     1) "base case")
  (is ( = (factorial 4)     (* 1 2 3 4)) "first recursion")
  (is ( = (factorial 10)    (apply * (range 1 10))) "general case" ))

(defn -init []
  (print "yo"))

(defn -factorial [this n]
  (factorial n))


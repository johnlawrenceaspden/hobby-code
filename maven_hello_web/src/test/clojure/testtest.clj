(ns testtest
  (:use clojure.test))

(println "Hello, I am a test script")

(deftest testme
  (println "testtest.testme running")
  (is true))

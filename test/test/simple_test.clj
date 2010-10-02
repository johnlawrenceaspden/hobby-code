(ns simple-test
  (:use clojure.test)
  (:use simple))

(deftest simple-test
  (is (=  (hello) "Hello world!"))
  (is (= (hello "test") "Hello test!")))
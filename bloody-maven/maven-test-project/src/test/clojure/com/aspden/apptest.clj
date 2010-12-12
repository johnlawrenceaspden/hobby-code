(ns com.aspden.apptest
  (:use clojure.test)
  (:use com.aspden.app)
  (:import com.aspden.App))


(deftest factorial-tests
  (testing "factorial"
    (testing "base cases"
      (is (= (factorial 1) 1))
      (is (= (factorial 2) 2))
      (is (= (factorial 0) 0)))
    (testing "general case"
      (is ( = (factorial 10) (apply * (range 1 11))))
      (is ( = (factorial 1000) (apply * (range 1 1001))) "big"))
    (testing "bad arguments"
      (is (thrown? java.lang.StackOverflowError (factorial 10000)) "not tail recursive")
      ;; deliberate fail. 
      (is (thrown? java.lang.StackOverflowError (factorial -1))))))

(deftest java-tests
  (testing "java factorial"
    ;;deliberate fail
    (is (= (. App factorial 10) (* 1 2 3 4 5 6 7 8 9)))
    (is (= (. App factorial 100) (apply * (range 1 101))) "arithmetic problems")))



;;OK, if I'm going to be doing this for more than toy programs I need to learn how to 
;;use the namespace and unit testing facilities

;;Here I'm typing at the repl, or more accurately I have this file open in emacs and I'm passing the 
;;expressions one by one to the repl.

(ns namespaces-and-tests
  (:use clojure.test))

;;here's a factorial, with some tests included

(with-test

    (defn factorial [n]
      (if (< n 3) n
          (* n (factorial (dec n)))))

  (is ( = (factorial 1)     1) "base case")
  (is ( = (factorial 4)     (* 1 2 3 4)) "first recursion")
  (is ( = (factorial 10)    (apply * (range 1 10))) "general case" ))


;;I can run my tests like so:
(run-tests)

;;or like
(run-tests 'namespaces-and-tests)


;;I can also define a separate factorial testing function, perhaps even in a different namespace
(ns separate-tests
  (:use clojure.test))

(use 'namespaces-and-tests);;however this breaks, whining something about classpaths and non-existent files
(ns-publics 'namespaces-and-tests) ;;even though we can still see the namespace and its function

(namespaces-and-tests/factorial 4) ;;and indeed it still works!

;;It appears that require defines libraries by filename, whilst refer talks about namespaces.
;;use is a combination of the two.

(refer 'namespaces-and-tests) ;;try this instead
(factorial 4)      ;;aha!


;;So here is a more complex test for the factorial
(deftest factorial-tests
  (testing "factorial"
    (testing "base cases"
      (is (= (factorial 1) 1))
      (is (= (factorial 2) 2))
      (is (= (factorial 0) 0)))
    (testing "general case"
      (is ( = (factorial 10) (apply * (range 1 11))))
      (is ( = (factorial 10000) (apply * (range 1 10001))) "tail recursion"))
    (testing "bad arguments"
      (is (thrown? java.lang.StackOverflowError (factorial -1)))
      (is (thrown? java.lang.StackOverflowError (factorial 0.5))))))

;;which we can run like this:
(run-tests 'separate-tests)

;;or like this
(run-tests)

;;we can run both at once with
(run-tests 'namespaces-and-tests 'separate-tests)

;;or we can use
(run-all-tests)
;;which claims to be testing everything in the clojure libraries, and in swank, which I'm using to connect clojure
;;to emacs, but doesn't actually seem to do any testing on them. Bug perhaps?
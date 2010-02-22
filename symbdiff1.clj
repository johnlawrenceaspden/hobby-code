;; The simplest possible symbolic differentiator

(defn make-add [ a b ] (list '+ a b))
(defn addition? [x] (and (=(count x) 3) (= (first x) '+)))
(defn add1   [x] (second x))
(defn add2   [x] (second (rest x)))

(defn make-mul [ a b ] (list '* a b))
(defn multiplication? [x] (and (=(count x) 3) (= (first x) '*)))
(defn mul1   [x] (second x))
(defn mul2   [x] (second (rest x)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (symbol? exp) (if (= exp var) 1 0)
        (addition? exp) (make-add (deriv (mul1 exp) var) (deriv (mul2 exp) var))
        (multiplication? exp) (make-add (make-mul (deriv (mul1 exp) var) (mul2 exp)) 
                                        (make-mul (mul1 exp) (deriv (mul2 exp) var)))
        :else :error))

(defn poly->fnform [poly] (list 'fn (vector 'x) poly))

;;examples of use

(def poly '(+ (+ (* x (* x x)) (* 2 (* x x))) 1))

(def polyfn  (eval (poly->fnform poly)))
(def dpolyfn (eval (poly->fnform (deriv poly 'x))))


;;tests

(use 'clojure.test)

(deftest deriv-test
  (testing "binary operators"
    (is (= (let [m '(* a b)] [(multiplication? m) (make-mul (mul1 m) (mul2 m))]) [true  '(* a b)]))
    (is (= (let [m '(* a b)] [(addition? m)       (make-add (add1 m) (add2 m))]) [false '(+ a b)])))
  (testing "derivative function"
    (is (= (deriv '0 'x)               '0))
    (is (= (deriv '1 'x)               '0))
    (is (= (deriv 'x 'x)               '1))
    (is (= (deriv 'y 'x)               '0))
    (is (= (deriv '(+ x x) 'x)         '(+ 1 1)))
    (is (= (deriv '(* x x) 'x)         '(+ (* 1 x) (* x 1))))
    (is (= (deriv '(* x x) 'y)         '(+ (* 0 x) (* x 0))))
    (is (= (deriv '(* x (* x x)) 'x)   '(+ (* 1 (* x x)) (* x (+ (* 1 x) (* x 1)))))))
  (testing "function creation: d/dx (x^3 + 2x^2 + 1) = 3x^2 + 4x "
    (let [poly '(+ (+ (* x (* x x)) (* 2 (* x x))) 1)]
      (is (= ((eval (poly->fnform poly)) 3) 46))
      (is (= ((eval (poly->fnform (deriv poly 'x))) 3))))))

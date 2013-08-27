;; Program to find square roots

(def ave (fn [x y] (/ (+ x y) 2)))

(def improve (fn [n guess] (ave guess (/ n guess))))

(def sq (fn [x] (* x x)))

(def abs (fn [x] (if (< (- x) 0) x (- x))))

(def check (fn [n x tol] (< (abs (- (sq x) n)) tol)))

(def root (fn [n x tol] (if (check n x tol) x (recur n (improve n x) tol))))


;; examples

(root 11 1. 0.01) ;; find sqrt 11 starting from 1 and make it good to 2 decimal places

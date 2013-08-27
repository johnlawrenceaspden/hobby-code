;; Program to find the square root of 11

(def abs (fn [x] (if (< (- x) 0) x (- x))))

(def improve (fn [guess] (ave guess (/ 11 guess))))

(def sq (fn [x] (* x x)))

(def ave (fn [x y] (/ (+ x y) 2)))

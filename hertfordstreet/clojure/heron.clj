(defn sum ([] 0)
	([x] x)
	([x & more ] (+ x (apply sum more))))

(defn average 
	([] 0)
	([x] x)
	([x & more] (/ (+ x (apply sum more)) (+ 1 (count more)))))
	

(defn abs [x] (if (< x 0) (- x) x))

(defn square [x] (* x x))

(defn improve [guess x] (average guess (/ x guess)))

(defn good-enough? [guess x] 
  (< (abs (- (square guess) x)) 0.001))

(defn try-sqrt [guess x]
  (if (good-enough? guess x) 
      guess
      (try-sqrt (improve guess x) x)))

(defn sqrt [x] (try-sqrt 1 x))
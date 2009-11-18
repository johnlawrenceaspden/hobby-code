(defn sum ([] 0)
	([x] x)
	([x & more ] (+ x (apply sum more))))

(defn average 
	([] 0)
	([x] x)
	([x & more] (/ (+ x (apply sum more)) (+ 1 (count more)))))
	

(defn abs [x] (if (< x 0) (- x) x))


(defn close-enough? [u v]
  (let [tolerance 0.000001]
    (< (abs (- u v)) tolerance)))

(defn fixed-point [f start]
  (loop [old start new (f start)]
    (prn old)
    (if (close-enough? old new)
	new
	(recur new (f new)))))


(defn average-damp [f]
  (fn [x] (average x (f x))))


(defn sqrt [x] (fixed-point (average-damp (fn [y] (/ x y))) 1))
(defn cubert [x] (fixed-point (average-damp (fn [y] (/ x y y))) 1))
(defn fourthrt [x] (fixed-point (average-damp (fn [y] (/ x y y y))) 1))



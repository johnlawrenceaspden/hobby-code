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
    (if (close-enough? old new)
	new
	(recur new (f new)))))


(defn sqrt [x] (fixed-point (fn [y] (average (/ x y) y)) 1))




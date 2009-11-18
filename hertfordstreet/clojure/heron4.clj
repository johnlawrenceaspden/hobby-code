(defn square [x] (* x x))

(defn deriv [f]
  (let [dx 0.000001]
    (fn [x] (/ (- (f (+ x dx)) (f x)) dx))))

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

(defn newton [f guess]
  (let [df (deriv f)]
    (fixed-point 
     (fn [x] 
	 (- x (/ (f x) (df x)))) 
     guess)))


(defn sqrt [a]     (newton (fn [x] (- (square x) a)) 1))
(defn cubert [a]   (newton (fn [x] (- (* x x x) a)) 1))
(defn fourthrt [a] (newton (fn [x] (- (* x x x x) a)) 1))







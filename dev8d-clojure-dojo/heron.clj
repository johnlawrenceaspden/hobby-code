;;Code from the first (impromptu) clojure dojo at dev8d. 
;;Written collaboratively by five people taking short turns.
;;We also did symbolic differentiation. That's in a separate file.

(defn abs [n]
  (if (< n 0)
    (- n)
    n))

(defn sqr [val]
  (* val val))

(defn avg [a b] (/ (+ a b) 2))

(defn it-impr [guess good-enough? impr] 
  (if (good-enough? guess) 
    guess 
    (it-impr (impr guess) good-enough? impr)))        

(defn make-good-enough [val] 
  (fn [guess] 
    (< (abs (- val (sqr guess)))
       (* 0.000001 val))))

(defn make-impr [val] 
  (fn [guess] 
    (avg (/ val guess) 
         guess)))

(defn sqrt [val] 
  (it-impr 1.0 (make-good-enough val) (make-impr val)))

(defn test-sqrt [] (map sqrt (list 9 16)) (list 3 4))

(reduce (fn [a b] (and a b))
        (map (fn [a b] 
               (< (abs (- a b)) 0.0001)) 
             (map sqrt (list 9 16)) 
             (list 3 4)))

(defn test-sqrt2 [x] (- (* (sqrt x) (sqrt x)) x))
(defn works? [x] (< (test-sqrt2 x) 0.1))
(works? 25) 



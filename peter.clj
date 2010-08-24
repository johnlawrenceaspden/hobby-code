(defn pyth [x y] (+ (* x x) (* y y)))

(defn av [x y] (/ (+ x y) 2))

(defn huron [x g] (av g (/ x g)))

(defn huron3 [x g] (av g (/ x (* g g))))

(defn fac [x] 
        (if (= x 0) 
          1 
          (* x (fac (- x 1)))))

(defn abs [x] (if (< x 0) (- x) x))


(defn huron2 [x g e] 
  (if (< (abs (- (* g g) x)) e)
    g (huron2 x (huron3 x g) e)))

(defn converged3? [x g e] 
  (<
    (abs
     (-
      (* g g g)
       x)) e )) 


(defn myfunc [x g e diff target] 
  (if (diff x g e) 
    g 
    (myfunc x (target x g) e diff target)))


(defn iterative-improve [x guess tol good-enough? improve] 
  (if (good-enough? x guess tol) 
    guess 
    (recur x (improve x guess) tol good-enough? improve)))

(defn near-sqrt? [x g e] 
  (< (abs (- (* g g) x)) e))

(defn improve-sqrt [x g] 
  (av g (/ x g)))

(defn square-root [n] 
  (iterative-improve n 1 0.000001 near-sqrt? improve-sqrt))

(square-root 81.0)




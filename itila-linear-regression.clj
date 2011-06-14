(def x [1 2 3])
(def y [4 4 7])
(defn square[x] (* x x))

(defn model[a c] (fn[x] (+ (* a x) c)))

(map - (map (model 3/2 2) x) y) ; (-1/2 1 -1/2)
(map - (map (model 7/3 1/3) x) y) ; (-4/3 1 1/3)

(reduce + (map square (map - (map (model 3/2 2) x) y))) ; 3/2
(reduce + (map square (map - (map (model 7/3 1/3) x) y))) ; 26/9

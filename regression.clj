(def data [[1,5], [25,15], [46,22], [76,32], [140,77]])
(def m 0.5) ; #'user/m

(def x (map first data)) ; #'user/x
(def y (map second data)) ; #'user/y

x ; (1 25 46 76 140) ; (1 25 46 76 140)
y ; (5 15 22 32 77) ; (5 15 22 32 77)

(def mean (float(/ (reduce + y) (count y)))) ; #'user/mean
mean ; 30.2 ; 30.2

(def variation (reduce + (map #(* % %) (map #(- % mean) y )))) ; #'user/variation
variation ; 3126.800000000003 ; 3126.800000000003


(def prediction (map (partial * m) x)) ; #'user/prediction
prediction ; (0.5 12.5 23.0 38.0 70.0) ; (0.6 15.0 27.599999999999998 45.6 84.0)
(def error (map - y prediction)) ; #'user/error
error ; (4.5 2.5 -1.0 -6.0 7.0) ; (4.4 0.0 -5.599999999999998 -13.600000000000001 -7.0)
(def errorsquared (map #(* % %) error)) ; #'user/errorsquared
errorsquared ; (20.25 6.25 1.0 36.0 49.0) ; (19.360000000000003 0.0 31.359999999999975 184.96000000000004 49.0)
(def sumerrorsquared (reduce + errorsquared)) ; #'user/sumerrorsquared
sumerrorsquared ; 112.5 ; 284.68
variation ; 3126.800000000003 ; 3126.800000000003

(- 1 (/ sumerrorsquared variation)) ; 0.9640207240629398 ;-> 0.9089548420110017
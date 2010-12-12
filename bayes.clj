; SLIME 20100404
user> (defn pdhm [m] (* (+ 1 (* m 0.3))(+ 1 (* m 0.5))(+ 1 (* m 0.7))(+ 1 (* m 0.8))(+ 1 (* m 0.9))))
#'user/pdhm
user> (pdhm 0.5)
3.9394687499999996
user> (pdhm 1)
11.3373
user> (pdhm -1)
0.0020999999999999994
user> (defn pdhm [m] (* (+ 1 (* m 0.3))(+ 1 (* m 0.5))(+ 1 (* m 0.7))(+ 1 (* m 0.8))(+ 1 (* m 0.9))))
#'user/pdhm
user> (map #(* m %) '(0.3 0.5 0.7 0.8 0.9))
; Evaluation aborted.
user> (map #(* -1 %) '(0.3 0.5 0.7 0.8 0.9))
(-0.3 -0.5 -0.7 -0.8 -0.9)
user> (map #(+ 1 (* -1 %)) '(0.3 0.5 0.7 0.8 0.9))
(0.7 0.5 0.30000000000000004 0.19999999999999996 0.09999999999999998)
user> (reduce * (map #(+ 1 (* -1 %)) '(0.3 0.5 0.7 0.8 0.9)))
0.0020999999999999994
user> (defn pdhm [m] (reduce * (map #(+ 1 (* -1 %)) '(0.3 0.5 0.7 0.8 0.9))))
#'user/pdhm
user> (pdhm 0.5)
0.0020999999999999994
user> (defn pdhm [m] (reduce * (map #(+ m (* -1 %)) '(0.3 0.5 0.7 0.8 0.9))))
#'user/pdhm
user> (pdhm 0.5)
-0.0
user> (defn pdhm [m] (reduce * (map #(+ 1 (* m %)) '(0.3 0.5 0.7 0.8 0.9))))
#'user/pdhm
user> (pdhm 0.5)
3.9394687499999996
user> (pdhm 1)
11.3373
user> (pdhm -1)
0.0020999999999999994
user> (defn pdhm [m] (reduce * (map #(/(+ 1 (* m %)) 2) '(0.3 0.5 0.7 0.8 0.9))))
#'user/pdhm
user> (pdhm -1)
6.562499999999998E-5
user> (pdhm 1)
0.354290625
user> (defn pdhm[data] (fn [m] (reduce * (map #(/(+ 1 (* m %)) 2) data))))
#'user/pdhm
user> (pdhm '(0.3 0.5 0.7 0.8 0.9))
#<user$pdhm$fn__9744 user$pdhm$fn__9744@118b62e>
user> ((pdhm '(0.3 0.5 0.7 0.8 0.9)) -1)
6.562499999999998E-5
user> ((pdhm '(0.3 0.5 0.7 0.8 0.9)) 1)
0.354290625
user> (defn pdh0[data] (fn [m] (reduce * (map (constantly 0.5) data))))
#'user/pdh0
user> ((pdh0 '(0.3 0.5 0.7 0.8 0.9)) 1)
0.03125
user> 
;; Whats the height of the unit regular tetrahedron in n dimensions?

;; degenerate case, 0 dimensions, it's a point, so its height is 1

;; opening case, 1 dimension, it's a line length 1

;; coords of vertices are
(def tet1 '((0)(1)))

;; first sensible case 2 dimensions, it's an equilateral triangle, based on case 1

;; How high? The height is from the median of the original points, 

(defn median [tet]
  (map #(/ % (count tet)) (apply map +  tet)))

(median tet1) ; (1/2)

;; By Pythagoras:

(defn height [median]
  (Math/sqrt (- 1 (reduce + (map #(* % %) median)))))

(height (median tet1)) ;-> 0.8660254037844386
(Math/sqrt (/ 3 4)) ;-> 0.8660254037844386

(map #(cons 0 %) tet1) ;-> ((0 0) (0 1))
(cons (height (median tet1)) (median tet1)) ;-> (0.8660254037844386 1/2)

(def tet2
  (cons 
   (cons (height (median tet1)) (median tet1))
   (map #(cons 0 %) tet1)))

tet2 ; ((0.8660254037844386 1/2) (0 0) (0 1))

(def tet3
  (cons 
   (cons (height (median tet2)) (median tet2))
   (map #(cons 0 %) tet2)))

tet3 ;-> ((0.816496580927726 0.28867513459481287 1/2) (0 0.8660254037844386 1/2) (0 0 0) (0 0 1))

(for [i tet3]
  (for [j tet3]
    (let [slength (reduce + (map #(* % %) (map - i j)))
          approx= (fn [a b] (< -0.000001 (- a b) 0.000001))]
      (if (= i j) (approx= 0 slength) (approx= 1 slength)))))
; ((true true true true) (true true true true) (true true true true) (true true true true))


(defn uptet [tet]
  (cons 
   (cons (height (median tet)) (median tet))
   (map #(cons 0 %) tet)))

(def tets (take 20 (iterate uptet tet1)))



(doseq [i tets]
  (doseq [j i]
    (println j )))

(doseq [i (map ffirst tets)]
  (print i "\n"))



;; Can we get the coordinates directly as a recurrence relation?

(average '(1 2 3 4)) ;-> 5/2



(defn average [lst] (/ (reduce + lst) (count lst)))
(defn sqsum [lst] (reduce + (map #(* % %) lst)))


(defn tet [row col]
  (cond (= [row col] [0 0]) 0
        (= [row col] [1 0]) 1
        (< row (inc col)) 0
        (> row (inc col)) (average (for [i (range row)] (tet i col)))
        (= row (inc col)) (Math/sqrt (- 1 (sqsum (for [i (range col)] (tet row i)))))))


(tet 1 0) ;-> 1
(Math/sqrt 3/4) ;-> 0.8660254037844386 ; height of equilateral triangle
(tet 2 1) ;-> 0.8660254037844386 
(Math/sqrt 2/3) ;-> 0.816496580927726  ; height of tetrahedron
(tet 3 2) ;-> 0.816496580927726
(Math/sqrt 5/8) ; 0.7905694150420949   ; height of hyper-tetrahedron
(tet 4 3) ;-> 0.7905694150420949
(Math/sqrt 3/5) ;-> 0.7745966692414834 ; height of super-hyper-tetrahedron
(tet 5 4) ;-> 0.7745966692414834
(Math/sqrt 7/12) ;-> 0.7637626158259733
(tet 6 5) ;-> 0.7637626158259733
(Math/sqrt 4/7) ;-> 0.7559289460184544
(tet 7 6) ; 0.7559289460184544
(Math/sqrt 9/16) ;-> 0.75
(tet 8 7) ; 0.75
(Math/sqrt 5/9) ;-> 0.7453559924999299
(tet 9 8) ;-> 0.7453559924999299



; 3/4 2/3 5/8 3/5 7/12 4/7 9/16 5/9
; looks much better as:
; 3/4 4/6 5/8 6/10 7/12 8/14 9/16 10/18

;; Conjecture that sqrt((n+1)/2*n) is the height of the n-dimensional tetrahedron


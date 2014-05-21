;; What's the height of the unit regular tetrahedron in n dimensions?

;; First, the one dimensional case is just a line, with vertices at 0 and 1

(def tet1 '((0)(1)))

;; The first non-degenerate case is in 2 dimensions, which is an equilateral triangle

;; You can make an equilateral triangle from the line above by adding
;; one more point above the centre of gravity of the line

(defn median [tet]
  (map #(/ % (count tet)) (apply map +  tet)))

(median tet1) ; (1/2)


;; How high should the point be?

;; By Pythagoras:

(defn height [median]
  (Math/sqrt (- 1 (reduce + (map #(* % %) median)))))

(height (median tet1)) ;-> 0.8660254037844386

;; Just to check, we know that the equilateral triangle is sqrt(3)/2 high
(Math/sqrt (/ 3 4)) ;-> 0.8660254037844386
;; Because the angles in an equilateral triangle are one-sixth of a turn
(Math/sin (/ (* 2 Math/PI) 6)) ;-> 0.8660254037844386

;; So we know enough to construct the coordinates for our triangle in 2-d
;; Mostly it's the one-d case with 0s in the new coordinates
(map #(cons 0 %) tet1) ;-> ((0 0) (0 1))
;; and there's one more point, over the middle of the base, as high as it needs to be 
;; to make its distance from the origin equal one
(cons (height (median tet1)) (median tet1)) ;-> (0.8660254037844386 1/2)

;; So the 2-d case, equilateral triangle, looks like
(def tet2
  (cons 
   (cons (height (median tet1)) (median tet1))
   (map #(cons 0 %) tet1)))

tet2 ; ((0.8660254037844386 1/2) (0 0) (0 1))

;; Similarly we can construct the tetrahedron in 3d like:
(def tet3
  (cons 
   (cons (height (median tet2)) (median tet2))
   (map #(cons 0 %) tet2)))

tet3 ;-> ((0.816496580927726 0.28867513459481287 1/2) (0 0.8660254037844386 1/2) (0 0 0) (0 0 1))

;; Just as a paranoid check, the distance between all these points should be one
(for [i tet3]
  (for [j tet3]
    (let [slength (reduce + (map #(* % %) (map - i j)))
          approx= (fn [a b] (< -0.000001 (- a b) 0.000001))]
      (if (= i j) (approx= 0 slength) (approx= 1 slength)))))
; ((true true true true) (true true true true) (true true true true) (true true true true))

;; So now, given a hyper-tetrahedron, we can construct the next one up
(defn uptet [tet]
  (cons 
   (cons (height (median tet)) (median tet))
   (map #(cons 0 %) tet)))

;; Let's make the first 20
(def tets (take 20 (iterate uptet tet1)))

;; here they are
(doseq [i tets]
  (doseq [j i]
    (println j )))

;; We can just look at the new heights introduced like:
(map ffirst (rest tets)) ;-> (0.8660254037844386 0.816496580927726 0.7905694150420949 0.7745966692414834 0.7637626158259734 0.7559289460184545 0.75 0.7453559924999299 0.7416198487095663 0.7385489458759964 0.7359800721939872 0.7337993857053428 0.7319250547113999 0.7302967433402214 0.7288689868556626 0.7276068751089989 0.7264831572567789 0.7254762501100117 0.724568837309472)

;; We know, from doing the first couple by hand, that these are always square roots of things
(map (comp #(* % %) ffirst) (rest tets)) ;-> (0.7499999999999999 0.6666666666666666 0.6250000000000001 0.6000000000000001 0.5833333333333334 0.5714285714285715 0.5625 0.5555555555555556 0.55 0.5454545454545454 0.5416666666666666 0.5384615384615384 0.5357142857142857 0.5333333333333332 0.53125 0.5294117647058824 0.5277777777777778 0.5263157894736843 0.525)

;; That looks awfully like (3/4 2/3 5/8 6/10 ...)

;; And *that* looks awfully like (3/4 4/6 5/8 6/10 ...)

(map #(/ (+ 2 %) (* 2 (inc %))) (range 1 20)) ;-> (3/4 2/3 5/8 3/5 7/12 4/7 9/16 5/9 11/20 6/11 13/24 7/13 15/28 8/15 17/32 9/17 19/36 10/19 21/40)

;; So I boldly conjecture that the height of a hyper-tetrahedron is sqrt(d+1/(2*d)))

(map -
     (map (comp #(* % %) ffirst) (rest tets))
     (map #(/ (inc %) (* 2 %)) (range 2 20))) 
;-> (-1.1102230246251565E-16 -1.1102230246251565E-16 1.1102230246251565E-16 1.1102230246251565E-16 1.1102230246251565E-16 1.1102230246251565E-16 0.0 0.0 0.0 -1.1102230246251565E-16 -1.1102230246251565E-16 -1.1102230246251565E-16 0.0 -1.1102230246251565E-16 0.0 0.0 0.0 1.1102230246251565E-16 0.0)

;; That's a bit too close to be coincidence.





;; Can we get the coordinates directly as a recurrence relation?

;; Doing it by hand suggests:

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


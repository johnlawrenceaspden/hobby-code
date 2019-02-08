;; Gridworld. It costs you one for every move, until you reach either NW or SE corner, trying to move off the grid
;; still costs one, but doesn't move you.

;; With a random policy, what is the value of each state?

;; t  1   2 3
;; 4  5   6 7
;; 8  9  10 11
;; 12 13 14 t

(defn drow [v cells ] (clojure.pprint/cl-format nil "| ~{~3d ~^| ~} |" (map v cells)))
(defn arow [v cells ] (clojure.pprint/cl-format nil "| ~{~6a ~^| ~} |" (map v cells)))

(def cells (cons :t (range 1 15)))
(defn co [n] [(quot n 4) (rem n 4)])
(defn oc [[row col]] (+ col (* row 4)))
(defn prim-adj [[row col]]  [[(inc row) col][(dec row) col][row (inc col)][row (dec col)]])
(defn crop [n]  (cond (< n 0) 0 (> n 3) 3 :else n))
(defn crop-adj [[row col]] (for [[r c] (prim-adj [row col])] [(crop r) (crop c)]))
(defn terminal [n]  (case n 0 :t 15 :t n))


(defn go-to [n]
  (if (= n :t) (list :t)
      (map terminal (map oc (crop-adj (co n))))))

(map go-to cells)

(def v (into {} (for [c cells] [c 0.0])))

(defn reward [n] (if (= n :t) 0 -1))

(defn up[v n]
  (let [g (go-to n)]
    (+ (reward n) (/ 
           (reduce + (map v (go-to n)))
           (count g)))))

(defn jacobi [v] (into {} (for [c cells] [c (up v c)])))

(jacobi v) ; {7 -1.0, 1 -1.0, 4 -1.0, 13 -1.0, 6 -1.0, 3 -1.0, 12 -1.0, 2 -1.0, 11 -1.0, 9 -1.0, 5 -1.0, :t 0.0, 14 -1.0, 10 -1.0, 8 -1.0}

(defn twosf   [x]  (float (/ (Math/round (* x 100.0)) 100)))
(def vfinal-j (into {} (for [[a b] (nth  (iterate jacobi v) 154)] [a (twosf b)])))

vfinal-j ; {7 -20.0, 1 -14.0, 4 -14.0, 13 -20.0, 6 -20.0, 3 -22.0, 12 -22.0, 2 -20.0, 11 -14.0, 9 -20.0, 5 -18.0, :t 0.0, 14 -14.0, 10 -18.0, 8 -20.0}

(arow vfinal-j [:t  1  2  3 ]) ; "| 0.0   | -14.0 | -20.0 | -22.0  |"
(arow vfinal-j [ 4  5  6  7 ]) ; "| -14.0 | -18.0 | -20.0 | -20.0  |"
(arow vfinal-j [ 8  9  10 11]) ; "| -20.0 | -20.0 | -18.0 | -14.0  |"
(arow vfinal-j [ 12 13 14 :t]) ; "| -22.0 | -20.0 | -14.0 | 0.0    |"


(def inplace (fn [v n] (assoc v n (up v n))))

(defn gauss-seidel [v] (reduce inplace v cells))

(gauss-seidel v) ; {7 -1.75, 1 -1.0, 4 -1.0, 13 -1.75, 6 -1.6875, 3 -1.3125, 12 -1.3125, 2 -1.25, 11 -1.8984375, 9 -1.6875, 5 -1.5, :t 0.0, 14 -1.8984375, 10 -1.84375, 8 -1.25}


(defn over-relax [v n omega]
  (let [a (v n)
        d (- (up v n) a)]
    (+ a (* omega d))))

(defn sor-inplace[omega]
  (fn [v n] (assoc v n (over-relax v n omega))))

(reduce (sor-inplace 1.1) v cells) ; {7 -2.0460687500000003, 1 -1.1, 4 -1.1, 13 -2.0460687500000003, 6 -1.9545625000000002, 3 -1.4856875, 12 -1.4856875, 2 -1.4025, 11 -2.260796484375, 9 -1.9545625000000002, 5 -1.7050000000000003, :t 0.0, 14 -2.260796484375, 10 -2.175009375, 8 -1.4025}

(defn sor[omega] (fn [v] (reduce (sor-inplace omega) v cells)))

(def vfinal-sor-2 (into {} (for [[a b] (nth  (iterate (sor 2.0) v) 16)] [a (twosf b)])) ) ; #'user/vfinal-sor-2

(arow vfinal-sor-2 [:t  1  2  3 ]) ; "| 0.0   | -14.0 | -20.0 | -22.0  |"
(arow vfinal-sor-2 [ 4  5  6  7 ]) ; "| -14.0 | -18.0 | -20.0 | -20.0  |"
(arow vfinal-sor-2 [ 8  9  10 11]) ; "| -20.0 | -20.0 | -18.0 | -14.0  |"
(arow vfinal-sor-2 [ 12 13 14 :t]) ; "| -22.0 | -20.0 | -14.0 | 0.0    |"


;; Modification, add state 15, from which transitions go to 12 13 14 and 15 itself.

;; t  1   2 3
;; 4  5   6 7
;; 8  9  10 11
;; 12 13 14 t
;;    15

;; v[15] = -1 + 1/4 (v[12,13,14,15])
;; 3 v[15] = -4 + (v[12]+v[13]+v[14])
;; v[15]   = -4/3 + average(v 12,13,14)

(def cells (cons :t (range 1 16)))

(defn gridworld-go-to [n]
  (if (= n :t) (list :t)
      (map terminal (map oc (crop-adj (co n))))))

(defn go-to [n]
  (if (= n 15) (list 12 13 14 15)
      (gridworld-go-to n)))

(def v (into {} (for [c cells] [c 0.0])))

(def vfinal-sor-2 (into {} (for [[a b] (nth  (iterate (sor 2.0) v) 16)] [a (twosf b)])) ) ; #'user/vfinal-sor-2

(arow vfinal-sor-2 [:t  1  2  3 ]) ; "| 0.0   | -14.0 | -20.0 | -22.0  |"
(arow vfinal-sor-2 [ 4  5  6  7 ]) ; "| -14.0 | -18.0 | -20.0 | -20.0  |"
(arow vfinal-sor-2 [ 8  9  10 11]) ; "| -20.0 | -20.0 | -18.0 | -14.0  |"
(arow vfinal-sor-2 [ 12 13 14 :t]) ; "| -22.0 | -20.0 | -14.0 | 0.0    |"
(arow vfinal-sor-2 [ :t 15 :t :t]) ; "| 0.0   | -20.0 | 0.0   | 0.0    |"

;; as predicted
(twosf (+ -4/3 (/ (+ -22 -20 -14) 3))) ; -20.0

;; Finally we change the dynamics so that down from state 13 goes to 15
(defn go-to [n]
  (cond (= n 15) (list 12 13 14 15)
        (= n 13) (list 9  12 14 15)
        :else (gridworld-go-to n)))


(def vfinal-sor-2 (into {} (for [[a b] (nth  (iterate (sor 2.0) v) 30)] [a (twosf b)])) ) ; #'user/vfinal-sor-2

(= vfinal-sor-2 ((sor 2.0) vfinal-sor-2)) ; true

(arow vfinal-sor-2 [:t  1  2  3 ]) ; "| 0.0    | -14.0  | -20.0  | -22.0   |"
(arow vfinal-sor-2 [ 4  5  6  7 ]) ; "| -14.0  | -18.0  | -20.0  | -20.0   |"
(arow vfinal-sor-2 [ 8  9  10 11]) ; "| -20.0  | -20.0  | -18.0  | -14.0   |"
(arow vfinal-sor-2 [ 12 13 14 :t]) ; "| -22.0  | -20.0  | -14.0  | 0.0     |"
(arow vfinal-sor-2 [ :t 15 :t :t]) ; "| 0.0    | -20.0  | 0.0    | 0.0     |"

;; Which makes surprisingly little difference

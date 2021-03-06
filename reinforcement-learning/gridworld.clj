;; Gridworld. It costs you one for every move, until you reach either NW or SE corner, trying to move off the grid
;; still costs one, but doesn't move you.

;; With a random policy, what is the value of each state?

;; t 1  2 3
;; 4 5  6 7
;; 8 9 10 11
;; 121314 t

(defn co [n]
  [(quot n 4) (rem n 4)])

(defn oc [[row col]]
  (+ col (* row 4)))

(map co (range 16)) ; ([0 0] [0 1] [0 2] [0 3] [1 0] [1 1] [1 2] [1 3] [2 0] [2 1] [2 2] [2 3] [3 0] [3 1] [3 2] [3 3])
(map (comp oc co) (range 16)) ; (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)

(defn prim-adj [[row col]]
  [[(inc row) col][(dec row) col][row (inc col)][row (dec col)]])

(prim-adj [2 2]) ; [[3 2] [1 2] [2 3] [2 1]]

(defn crop [n]
  (cond (< n 0) 0
        (> n 3) 3
        :else n))

(defn crop-adj [[row col]]
  (for [[r c] (prim-adj [row col])] [(crop r) (crop c)]))

(map oc (crop-adj (co 7))) ; (11 3 7 6)

(defn terminal [n]
  (case n
    0  :t
    15 :t
    n))

(defn go-to [n]
  (if (= n :t) (list :t)
      (map terminal (map oc (crop-adj (co n))))))

(def cells (cons :t (range 1 15)))

(map go-to cells) ; ((:t) (5 1 2 :t) (6 2 3 1) (7 3 3 2) (8 :t 5 4) (9 1 6 4) (10 2 7 5) (11 3 7 6) (12 4 9 8) (13 5 10 8) (14 6 11 9) (:t 7 11 10) (12 8 13 12) (13 9 14 12) (14 10 :t 13))

;; t 1  2 3
;; 4 5  6 7
;; 8 9 10 11
;; 121314 t

;; 9 -> 5 8 10 13
(go-to 9) ; (13 5 10 8)

;; 1 -> t 2 5 1
(go-to 1) ; (5 1 2 :t)

;; recurrence for value functions under the random policy gives us
;; v[9]=-1 + 1/4(v[9] + v[9] + v[9] +v [9])
;; v[:t] = v[:t]
;; v[4] = -1 + 1/4 (v[4] + v[5] + v[8] + v[:t])
;; etc

;; So let's take an initial value function:
(def v (into {} (for [c cells] [c 0.0])))

;; and use the RHS of the recurrence
(defn reward [n] (if (= n :t) 0 -1))

(defn up[v n]
  (let [g (go-to n)]
    (+ (reward n) (/ 
           (reduce + (map v (go-to n)))
           (count g)))))

;; To make an update rule
;; v[4] <- -1 + 1/4 (v[4] + v[5] + v[8] + v[:t])

(up v 1) ; -1.0
(up v :t) ; 0.0

(into {} (for [c cells] [c (up v c)])) ; {7 -1.0, 1 -1.0, 4 -1.0, 13 -1.0, 6 -1.0, 3 -1.0, 12 -1.0, 2 -1.0, 11 -1.0, 9 -1.0, 5 -1.0, :t 0.0, 14 -1.0, 10 -1.0, 8 -1.0}

(def v2 (into {} (for [c cells] [c (up v c)])))

(into {} (for [c cells] [c (up v2 c)])) ; {7 -2, 1 -7/4, 4 -7/4, 13 -2, 6 -2, 3 -2, 12 -2, 2 -2, 11 -7/4, 9 -2, 5 -2, :t 0, 14 -7/4, 10 -2, 8 -2}

(defn jacobi [v] (into {} (for [c cells] [c (up v c)])))

(defn twosf   [x]  (float (/ (Math/round (* x 100.0)) 100)))

(jacobi v) ; {7 -1, 1 -1, 4 -1, 13 -1, 6 -1, 3 -1, 12 -1, 2 -1, 11 -1, 9 -1, 5 -1, :t 0, 14 -1, 10 -1, 8 -1}
(for [[a b] (nth  (iterate jacobi v) 3)] (twosf b)) ; (-2.94 -2.44 -2.44 -2.94 -3.0 -3.0 -3.0 -2.94 -2.44 -3.0 -2.87 0.0 -2.44 -2.87 -2.94)
(for [[a b] (nth  (iterate jacobi v) 30)] (twosf b)) ; (-16.1 -11.37 -11.37 -16.1 -16.12 -17.63 -17.63 -16.1 -11.37 -16.12 -14.56 0.0 -11.37 -14.56 -16.1)
(for [[a b] (nth  (iterate jacobi v) 60)] (twosf b)) ; (-19.24 -13.49 -13.49 -19.24 -19.25 -21.15 -21.15 -19.24 -13.49 -19.25 -17.33 0.0 -13.49 -17.33 -19.24)
(for [[a b] (nth  (iterate jacobi v) 90)] (twosf b)) ; (-19.85 -13.9 -13.9 -19.85 -19.85 -21.84 -21.84 -19.85 -13.9 -19.85 -17.87 0.0 -13.9 -17.87 -19.85)
(for [[a b] (nth  (iterate jacobi v) 120)] (twosf b)) ; (-19.97 -13.98 -13.98 -19.97 -19.97 -21.97 -21.97 -19.97 -13.98 -19.97 -17.97 0.0 -13.98 -17.97 -19.97)
(for [[a b] (nth  (iterate jacobi v) 150)] (twosf b)) ; (-19.99 -14.0 -14.0 -19.99 -19.99 -21.99 -21.99 -19.99 -14.0 -19.99 -18.0 0.0 -14.0 -18.0 -19.99)
(for [[a b] (nth  (iterate jacobi v) 180)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate jacobi v) 170)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate jacobi v) 160)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate jacobi v) 155)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate jacobi v) 152)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -21.99 -21.99 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate jacobi v) 151)] (twosf b)) ; (-19.99 -14.0 -14.0 -19.99 -19.99 -21.99 -21.99 -19.99 -14.0 -19.99 -18.0 0.0 -14.0 -18.0 -19.99)


;; At this point, truncating to two sf gets the actual answer!
(def vfinal-j (into {} (for [[a b] (nth  (iterate jacobi v) 154)] [a (twosf b)])))
(= vfinal-j (jacobi vfinal-j)) ; true


vfinal-j ; {7 -20.0, 1 -14.0, 4 -14.0, 13 -20.0, 6 -20.0, 3 -22.0, 12 -22.0, 2 -20.0, 11 -14.0, 9 -20.0, 5 -18.0, :t 0.0, 14 -14.0, 10 -18.0, 8 -20.0}


(def inplace (fn [v n] (assoc v n (up v n))))

(inplace v 1) ; {7 0, 1 -1, 4 0, 13 0, 6 0, 3 0, 12 0, 2 0, 11 0, 9 0, 5 0, :t 0, 14 0, 10 0, 8 0}

(go-to 1) ; (5 1 2 :t)

(map v (go-to 1)) ; (0 0 0 0)

(inplace v :t) ; {7 0, 1 0, 4 0, 13 0, 6 0, 3 0, 12 0, 2 0, 11 0, 9 0, 5 0, :t 0, 14 0, 10 0, 8 0}

(reduce inplace v cells) ; {7 -7/4, 1 -1, 4 -1, 13 -7/4, 6 -27/16, 3 -21/16, 12 -21/16, 2 -5/4, 11 -243/128, 9 -27/16, 5 -3/2, :t 0, 14 -243/128, 10 -59/32, 8 -5/4}
(reduce inplace (reduce inplace v cells) cells) ; {7 -1743/512, 1 -31/16, 4 -31/16, 13 -1743/512, 6 -829/256, 3 -699/256, 12 -699/256, 2 -163/64, 11 -3295/1024, 9 -829/256, 5 -45/16, :t 0, 14 -3295/1024, 10 -1827/512, 8 -163/64}

(defn gauss-seidel [v] (reduce inplace v cells))

(gauss-seidel v) ; {7 -1.75, 1 -1.0, 4 -1.0, 13 -1.75, 6 -1.6875, 3 -1.3125, 12 -1.3125, 2 -1.25, 11 -1.8984375, 9 -1.6875, 5 -1.5, :t 0.0, 14 -1.8984375, 10 -1.84375, 8 -1.25}
(nth (iterate gauss-seidel v) 10) ; {7 -11.8633986227901, 1 -7.82506756017392, 4 -7.82506756017392, 13 -11.8633986227901, 6 -11.769337388334861, 3 -12.227655033372685, 12 -12.227655033372685, 2 -11.121818717405404, 11 -8.81387888655695, 9 -11.769337388334861, 5 -10.420371658008662, :t 0.0, 14 -8.81387888655695, 10 -11.053818103924186, 8 -11.121818717405404}
(for [[a b] (nth  (iterate gauss-seidel v) 3)] (twosf b)) ; (-4.88 -2.82 -2.82 -4.88 -4.71 -4.18 -4.18 -3.83 -4.26 -4.71 -4.03 0.0 -4.26 -4.96 -3.83)
(for [[a b] (nth  (iterate gauss-seidel v) 30)] (twosf b)) ; (-18.59 -12.93 -12.93 -18.59 -18.57 -20.3 -20.3 -18.46 -13.1 -18.57 -16.68 0.0 -13.1 -16.79 -18.46)
(for [[a b] (nth  (iterate gauss-seidel v) 60)] (twosf b)) ; (-19.9 -13.92 -13.92 -19.9 -19.9 -21.88 -21.88 -19.89 -13.93 -19.9 -17.9 0.0 -13.93 -17.91 -19.89)
(for [[a b] (nth  (iterate gauss-seidel v) 90)] (twosf b)) ; (-19.99 -13.99 -13.99 -19.99 -19.99 -21.99 -21.99 -19.99 -14.0 -19.99 -17.99 0.0 -14.0 -17.99 -19.99)
(for [[a b] (nth  (iterate gauss-seidel v) 120)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate gauss-seidel v) 100)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate gauss-seidel v) 95)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -21.99 -21.99 -19.99 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -19.99)
(for [[a b] (nth  (iterate gauss-seidel v) 96)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -21.99 -21.99 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)

(def vfinal-gs (into {} (for [[a b] (nth  (iterate gauss-seidel v) 97)] [a (twosf b)])) )
(= vfinal-gs (gauss-seidel vfinal-gs)) ; true

vfinal-gs ; {7 -20.0, 1 -14.0, 4 -14.0, 13 -20.0, 6 -20.0, 3 -21.99, 12 -21.99, 2 -20.0, 11 -14.0, 9 -20.0, 5 -18.0, :t 0.0, 14 -14.0, 10 -18.0, 8 -20.0}

(= vfinal-gs vfinal-j) ; true

(defn over-relax [v n omega]
  (let [a (v n)
        d (- (up v n) a)]
    (+ a (* omega d))))

(v 1) ; 0.0
(up v 1) ; -1.0
(over-relax v 1 1.1) ; -1.1

(defn sor-inplace[omega]
  (fn [v n] (assoc v n (over-relax v n omega))))

(reduce (sor-inplace 1.1) v cells) ; {7 -2.0460687500000003, 1 -1.1, 4 -1.1, 13 -2.0460687500000003, 6 -1.9545625000000002, 3 -1.4856875, 12 -1.4856875, 2 -1.4025, 11 -2.260796484375, 9 -1.9545625000000002, 5 -1.7050000000000003, :t 0.0, 14 -2.260796484375, 10 -2.175009375, 8 -1.4025}

(defn sor[omega] (fn [v] (reduce (sor-inplace omega) v cells)))

(for [[a b] (nth  (iterate (sor 1.1) v) 100)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate (sor 1.1) v) 50)] (twosf b)) ; (-19.89 -13.91 -13.91 -19.89 -19.89 -21.86 -21.86 -19.88 -13.93 -19.89 -17.89 0.0 -13.93 -17.9 -19.88)
(for [[a b] (nth  (iterate (sor 1.1) v) 75)] (twosf b)) ; (-19.99 -13.99 -13.99 -19.99 -19.99 -21.99 -21.99 -19.99 -13.99 -19.99 -17.99 0.0 -13.99 -17.99 -19.99)
(for [[a b] (nth  (iterate (sor 1.1) v) 87)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate (sor 1.1) v) 81)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -21.99 -21.99 -19.99 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -19.99)
(for [[a b] (nth  (iterate (sor 1.1) v) 82)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -21.99 -21.99 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate (sor 1.1) v) 83)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)




(def vfinal-sor-11 (into {} (for [[a b] (nth  (iterate (sor 1.1) v) 83)] [a (twosf b)])) ) ; #'user/vfinal-sor-11
vfinal-sor-11 ; {7 -20.0, 1 -14.0, 4 -14.0, 13 -20.0, 6 -20.0, 3 -22.0, 12 -22.0, 2 -20.0, 11 -14.0, 9 -20.0, 5 -18.0, :t 0.0, 14 -14.0, 10 -18.0, 8 -20.0} ; 
(= vfinal-sor-11 ((sor 1.1) vfinal-sor-11)) ; true 
(= vfinal-sor-11 vfinal-j) ; true

(for [[a b] (nth  (iterate (sor 2.0) v) 10)] (twosf b)) ; (-19.98 -13.93 -13.93 -19.98 -19.98 -21.99 -21.99 -19.98 -13.99 -19.98 -18.04 0.0 -13.99 -18.0 -19.98)
(for [[a b] (nth  (iterate (sor 3.0) v) 10)] (twosf b)) ; (-29.54 330.99 330.99 -29.54 104.28 236.57 236.57 265.2 -349.69 104.28 85.92 0.0 -349.69 -215.5 265.2)
(for [[a b] (nth  (iterate (sor 2.5) v) 10)] (twosf b)) ; (-24.97 -5.55 -5.55 -24.97 -23.05 -21.02 -21.02 -17.95 -21.99 -23.05 -24.07 0.0 -21.99 -26.8 -17.95)

(for [[a b] (nth  (iterate (sor 2.0) v) 20)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate (sor 2.0) v) 15)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -17.99 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate (sor 2.0) v) 16)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)

(def vfinal-sor-2 (into {} (for [[a b] (nth  (iterate (sor 2.0) v) 16)] [a (twosf b)])) ) ; #'user/vfinal-sor-2
(= vfinal-sor-2 ((sor 2.0) vfinal-sor-2)) ; true
(= vfinal-sor-2 vfinal-j) ; true

(for [[a b] (nth  (iterate (sor 2.1) v) 16)] (twosf b)) ; (-19.99 -13.98 -13.98 -19.99 -19.99 -22.0 -22.0 -19.99 -14.0 -19.99 -18.02 0.0 -14.0 -18.0 -19.99)
(for [[a b] (nth  (iterate (sor 1.9) v) 16)] (twosf b)) ; (-19.99 -13.99 -13.99 -19.99 -19.99 -21.99 -21.99 -19.99 -14.0 -19.99 -17.99 0.0 -14.0 -17.99 -19.99)
(for [[a b] (nth  (iterate (sor 2.01) v) 16)] (twosf b)) ; (-20.0 -13.99 -13.99 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)
(for [[a b] (nth  (iterate (sor 1.99) v) 16)] (twosf b)) ; (-20.0 -14.0 -14.0 -20.0 -20.0 -22.0 -22.0 -20.0 -14.0 -20.0 -18.0 0.0 -14.0 -18.0 -20.0)

(def a )

(defn l2error [ method its ]
  (Math/sqrt
   (reduce +
           (map #(* % %)
                (for [c cells] (-
                                ((nth  (iterate method v) its) c)
                                (vfinal-j c)))))))



(l2error (sor 2.0) 16) ; 0.007775195519450396
(l2error (sor 1.9) 16) ; 0.0346695210788383
(l2error (sor 1.95) 16) ; 0.008210797893234447
(l2error (sor 1.97) 16) ; 0.005842332536781981
(l2error (sor 1.98) 16) ; 0.005949376660155789
(l2error (sor 1.96) 16) ; 0.006534845813183601

(l2error jacobi 16) ; 28.88787060168189 
(l2error gauss-seidel 16) ; 17.26740892000507
(l2error (sor 2.0) 16) ; 0.007775195519450396

(l2error jacobi 32) ; 12.046925440162795 
(l2error gauss-seidel 32) ; 4.256041879879601
(l2error (sor 2.0) 32) ; 9.110653309046185E-6

(l2error jacobi 64) ; 2.0950655094988813 
(l2error gauss-seidel 64) ; 0.25856064270263823
(l2error (sor 2.0) 64) ; 5.1711589578882766E-11


(Math/log (l2error jacobi 16)) ; 3.3634218046547497 
(Math/log (l2error gauss-seidel 16)) ; 2.848820847318467 
(Math/log (l2error (sor 2.0) 16)) ; -4.856816674036832 

(Math/log (l2error jacobi 32)) ; 2.4888094771863565 
(Math/log (l2error gauss-seidel 32)) ; 1.4483395921882034 
(Math/log (l2error (sor 2.0) 32)) ; -11.606066135866744 

(Math/log (l2error jacobi 64)) ; 0.739584822335762 
(Math/log (l2error gauss-seidel 64)) ; -1.352625017952317 
(Math/log (l2error (sor 2.0) 64)) ; -23.68533918973215

(Math/log (l2error (sor 2.0) 64)) ; -23.68533918973215
(Math/log (l2error (sor 2.01) 64)) ; -22.94478752035312
(Math/log (l2error (sor 1.99) 64)) ; -24.43391328867895
(Math/log (l2error (sor 1.9) 64)) ; -27.40879324465447
(Math/log (l2error (sor 1.8) 64)) ; -19.238218999163557
(Math/log (l2error (sor 1.91) 64)) ; -28.59367117870677
(Math/log (l2error (sor 1.95) 64)) ; -27.51396234364307
(Math/log (l2error (sor 1.92) 64)) ; -29.403457268302535
(Math/log (l2error (sor 1.93) 64)) ; -29.088927124381613 ; -29.403457268302535

(Math/log (l2error (sor 1.92) 32)) ; -12.55905214406047
(Math/log (l2error (sor 1.92) 16)) ; -3.908927723163238

(l2error (sor 1.92) 16) ; 0.020062001551948344
(l2error (sor 1.92) 17) ; 0.010567420280940409
(l2error (sor 1.92) 18) ; 0.006737196437858306
(l2error (sor 1.92) 19) ; 0.0036875430427638495

(/ (l2error (sor 1.92) 18)
   (l2error (sor 1.92) 19)) ; 1.827014996090381

(/ (l2error (sor 1.92) 19)
   (l2error (sor 1.92) 20)) ; 1.640640077410825

(/ (l2error (sor 1.92) 20)
   (l2error (sor 1.92) 21)) ; 1.8204422041389874

(/ (l2error (sor 1.92) 21)
   (l2error (sor 1.92) 22)) ; 1.579364215498948

(defn ratio [method its]
  (/ (l2error method its)
     (l2error method (inc its))))


(for [i (range 30 40)] (twosf (ratio jacobi i))) ; (1.06 1.06 1.06 1.06 1.06 1.06 1.06 1.06 1.06 1.06)
(for [i (range 30 40)] (twosf (ratio gauss-seidel i))) ; (1.09 1.09 1.09 1.09 1.09 1.09 1.09 1.09 1.09 1.09)
(for [i (range 30 40)] (twosf (ratio (sor 1.1) i))) ; (1.11 1.11 1.11 1.11 1.11 1.11 1.11 1.11 1.11 1.11)
(for [i (range 30 40)] (twosf (ratio (sor 1.2) i))) ; (1.13 1.13 1.13 1.13 1.13 1.13 1.13 1.13 1.13 1.13)
(for [i (range 30 40)] (twosf (ratio (sor 1.3) i))) ; (1.15 1.15 1.15 1.15 1.15 1.15 1.15 1.15 1.15 1.15)
(for [i (range 30 40)] (twosf (ratio (sor 1.4) i))) ; (1.18 1.18 1.18 1.18 1.18 1.18 1.18 1.18 1.18 1.18)
(for [i (range 30 40)] (twosf (ratio (sor 1.5) i))) ; (1.22 1.22 1.22 1.22 1.22 1.22 1.22 1.22 1.22 1.22)
(for [i (range 30 40)] (twosf (ratio (sor 1.6) i))) ; (1.27 1.27 1.27 1.27 1.27 1.27 1.27 1.27 1.27 1.27)
(for [i (range 30 40)] (twosf (ratio (sor 1.7) i))) ; (1.34 1.34 1.34 1.34 1.34 1.34 1.34 1.34 1.34 1.34)
(for [i (range 30 40)] (twosf (ratio (sor 1.8) i))) ; (1.45 1.45 1.45 1.45 1.45 1.45 1.45 1.45 1.45 1.45)
(for [i (range 30 40)] (twosf (ratio (sor 1.9) i))) ; (1.67 1.63 1.67 1.63 1.67 1.63 1.67 1.63 1.67 1.63)
(for [i (range 30 40)] (twosf (ratio (sor 2.0) i))) ; (1.53 1.41 1.39 1.44 1.52 1.53 1.48 1.43 1.43 1.46)
(for [i (range 30 40)] (twosf (ratio (sor 2.1) i))) ; (1.48 1.27 1.19 1.24 1.35 1.43 1.37 1.27 1.24 1.28)
(for [i (range 30 40)] (twosf (ratio (sor 2.2) i))) ; (1.45 1.18 1.05 1.06 1.2 1.34 1.31 1.17 1.09 1.12)
(for [i (range 30 40)] (twosf (ratio (sor 2.3) i))) ; (1.54 1.04 0.97 0.93 1.02 1.29 1.25 1.08 0.98 0.97)

(for [i (range 30 40)] (twosf (ratio (sor 1.85) i))) ; (1.53 1.53 1.53 1.53 1.53 1.53 1.53 1.53 1.53 1.53)
(for [i (range 30 40)] (twosf (ratio (sor 1.9) i))) ; (1.67 1.63 1.67 1.63 1.67 1.63 1.67 1.63 1.67 1.63)
(for [i (range 30 40)] (twosf (ratio (sor 1.95) i))) ; (2.04 1.24 1.79 1.35 1.82 1.44 1.69 1.43 1.63 1.49)
(for [i (range 30 40)] (twosf (ratio (sor 2.00) i))) ; (1.53 1.41 1.39 1.44 1.52 1.53 1.48 1.43 1.43 1.46)

(for [i (range 30 40)] (twosf (ratio (sor 1.9) i))) ; (1.67 1.63 1.67 1.63 1.67 1.63 1.67 1.63 1.67 1.63)
(for [i (range 30 40)] (twosf (ratio (sor 1.925) i))) ; (2.03 1.47 2.1 1.4 2.17 1.38 2.18 1.35 2.27 1.28)
(for [i (range 30 40)] (twosf (ratio (sor 1.95) i))) ; (2.04 1.24 1.79 1.35 1.82 1.44 1.69 1.43 1.63 1.49)


(l2error (sor 1.9)  100) ; 1.5587468005037626E-14
(l2error (sor 1.95) 100) ; 2.0791723739534308E-14
(l2error (sor 2.00) 100) ; 1.7763568394002505E-14

(l2error (sor 1.9)  200) ; 1.7130564986077266E-14
(l2error (sor 1.95) 200) ; 2.0791723739534308E-14
(l2error (sor 2.00) 200) ; 1.7763568394002505E-14

(l2error (sor 1.9)  50) ; 1.3773805744680698E-9 
(l2error (sor 1.95) 50) ; 5.059378578132793E-10 
(l2error (sor 2.00) 50) ; 9.94067570355995E-9

(def valuemap (into {} (for [[k val] (nth (iterate (sor 1.9) v) 40)] [k (twosf val)])))


(defn arow [v cells ]
  (clojure.pprint/cl-format nil "| ~{~5a ~^| ~} |" (map v cells)))

(arow valuemap [:t  1  2  3 ]) ; "| 0.0   | -14.0 | -20.0 | -22.0  |"
(arow valuemap [ 4  5  6  7 ]) ; "| -14.0 | -18.0 | -20.0 | -20.0  |"
(arow valuemap [ 8  9  10 11]) ; "| -20.0 | -20.0 | -18.0 | -14.0  |"
(arow valuemap [ 12 13 14 :t]) ; "| -22.0 | -20.0 | -14.0 | 0.0    |"
(arow valuemap [ :t 15 :t :t]) ; "| 0.0   | nil   | 0.0   | 0.0    |"


(defn drow [v cells ]
  (clojure.pprint/cl-format nil "| ~{~3d ~^| ~} |" (map v cells)))

(drow valuemap [:t  1  2  3 ]) ; "|   0 | -14 | -20 | -22  |"
(drow valuemap [ 4  5  6  7 ]) ; "| -14 | -18 | -20 | -20  |"
(drow valuemap [ 8  9  10 11]) ; "| -20 | -20 | -18 | -14  |"
(drow valuemap [ 12 13 14 :t]) ; "| -22 | -20 | -14 |   0  |"
(drow valuemap [ :t 15 :t :t]) ; "|   0 | nil |   0 |   0  |"





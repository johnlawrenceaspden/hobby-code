;; This is broken and needs fixing and tidying up

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

(defn cut [n]
  (cond (< n 0) 0
        (> n 3) 3
        :else n))

(defn cut* [[row col]]
  [(cut row)(cut col)])

(defn periodic-adj [[row col]]
  (map cut* (prim-adj [row col])))

(map oc (periodic-adj (co 7))) ; (11 3 7 6)

(defn t [n]
  (case n
    0  :t
    15 :t
    n))

(defn go-to [n]
  (if (= n :t) (list n)
      (map t (map oc (periodic-adj (co n))))))

(map go-to (cons :t (range 1 15))) ; ((:t) (5 1 2 :t) (6 2 3 1) (7 3 3 2) (8 :t 5 4) (9 1 6 4) (10 2 7 5) (11 3 7 6) (12 4 9 8) (13 5 10 8) (14 6 11 9) (:t 7 11 10) (12 8 13 12) (13 9 14 12) (14 10 :t 13))

;; t 1  2 3
;; 4 5  6 7
;; 8 9 10 11
;; 121314 t

;; 9 -> 5 8 10 13
(go-to 9) ; (13 5 10 8)

;; 1 -> t 2 5 1
(go-to 1) ; (5 1 2 :t)

;; recurrence gives us
;; v[1]=-1 + 1/4(v[5] + v[1] + v[2] +v [t])


(def cells (cons :t (range 1 15)))


(def v (into {} (for [c cells] [c 0]))) ; {7 0, 1 0, 4 0, 13 0, 6 0, 3 0, 12 0, 2 0, 11 0, 9 0, 5 0, :t 0, 14 0, 10 0, 8 0}

(defn reward [n] (if (= n :t) 0 -1))

(defn up[v n]
  (let [g (go-to n)]
    (+ (reward n) (/ 
           (reduce + (map v (go-to n)))
           (count g)))))

(up v 1)
(up v :t)

(def inplace (fn [v n] (assoc v n (up v n))))

(inplace v 1) ; {7 0, 1 -1, 4 0, 13 0, 6 0, 3 0, 12 0, 2 0, 11 0, 9 0, 5 0, :t 0, 14 0, 10 0, 8 0}

(go-to 1) ; (5 1 2 :t)

(map v (go-to 1)) ; (0 0 0 0)

(inplace v :t) ; {7 0, 1 0, 4 0, 13 0, 6 0, 3 0, 12 0, 2 0, 11 0, 9 0, 5 0, :t 0, 14 0, 10 0, 8 0}

(reduce inplace v cells) ; {7 -7/4, 1 -1, 4 -1, 13 -7/4, 6 -27/16, 3 -21/16, 12 -21/16, 2 -5/4, 11 -243/128, 9 -27/16, 5 -3/2, :t 0, 14 -243/128, 10 -59/32, 8 -5/4}
(reduce inplace (reduce inplace v cells) cells) ; {7 -1743/512, 1 -31/16, 4 -31/16, 13 -1743/512, 6 -829/256, 3 -699/256, 12 -699/256, 2 -163/64, 11 -3295/1024, 9 -829/256, 5 -45/16, :t 0, 14 -3295/1024, 10 -1827/512, 8 -163/64}
(reduce inplace (reduce inplace v cells) cells)
(def f (reduce inplace (reduce inplace (reduce inplace v cells) cells) cells))

(defn twosf   [x]  (float (/ (Math/round (* x 100.0)) 100)))

(for [[k val] f] [k (twosf val)]) ; ([7 -4.88] [1 -2.82] [4 -2.82] [13 -4.88] [6 -4.71] [3 -4.18] [12 -4.18] [2 -3.83] [11 -4.26] [9 -4.71] [5 -4.03] [:t 0.0] [14 -4.26] [10 -4.96] [8 -3.83])


          





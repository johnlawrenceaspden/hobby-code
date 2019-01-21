(defn table [g]
  (let [g2 (/ 1 (- 1 (* g g)))]
    [ g2 (* 2 g g2)
      (* g g2) (* 2 g g g2)
      (+ 2 (* g g2)) (* 2 g2)]))


(table 0.5) ; [[1.3333333333333333 1.3333333333333333 0.6666666666666666 0.6666666666666666 2.6666666666666665 2.6666666666666665]]

(table 0.4) ; [[1.1904761904761905 0.9523809523809524 0.4761904761904762 0.38095238095238104 2.4761904761904763 2.380952380952381]]

(map < (table 0.5) (table 0.4)) ; (false false false false false false)

(map < (table 0.0) (table 0.9)) ; (true true true true true true)


(defn opt [t]
  (let [reds (map (fn [[a b]](< a b)) (partition 2 t))]
  (cond (= reds (list false false false)) :>
        (= reds (list true  true  true))  :<
        :else :!)))
      
(opt (table 0.4))
(opt (table 0.3))

(map opt (map table (range 0 0.99 0.01))) ; (:> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :> :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :< :<)

(distinct (map opt (map table (range 0 0.999 0.001))))


(reduce #(and %1 %2) )

(defn comp [t1 t2]
  (if (false (reduce #(and %1 %2) (map < t1 t2))) :












(for [[a b] in (partition (table 0))]) ; [[1 0 0 0 2 2]]



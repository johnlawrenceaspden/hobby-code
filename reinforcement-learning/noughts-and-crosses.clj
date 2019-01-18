;; How many noughts and crosses positions are there?

;; Naive calculation, 9 positions, all can be x,0,_, so 
(* 3 3 3 3 3 3 3 3 3) ; 19683
;; 20000 possible board
;; eight member symmetry group, most positions have eight copies
(float (/ (* 3 3 3 3 3 3 3 3 3) 8)) ; 2460.375



;; wlog say x starts : must be either even o,x or 1 more x than o
(reduce +
        (list 9
              (* 9 8)
              (/ (* 9 8 7) 2)
              (/ (* 9 8 7 6) 2 2)
              (/ (* 9 8 7 6 5) 2 (* 2 3))
              (/ (* 9 8 7 6 5 4) (* 2 3) (* 2 3))
              (/ (* 9 8 7 6 5 4 3) (* 2 3 4) (* 2 3))
              (/ (* 9 8 7 6 5 4 3 2) (* 2 3 4) (* 2 3 4))
              (/ (* 9 8 7 6 5 4 3 2 1) (* 2 3 4 5) (* 2 3 4)))) ; 6045

;; again, symmetries
(float (/ 6045 8)) ; 755.625

;; all these numbers are tractable

(defn spin [v]
  (mapv v (map dec [7 4 1 8 5 2 9 6 3])))

(defn mirror [v]
  (mapv v (map dec [3 2 1 6 5 4 9 8 7])))

(defn symmetries [v]
  (list 
   v
   (spin v)
   (spin (spin v))
   (spin (spin (spin v)))
   (mirror v)
   (mirror (spin v))
   (mirror (spin (spin v)))
   (mirror (spin (spin (spin v))))
   ))


(defn canonical-representation [v]
  (first (sort (symmetries v))))

(canonical-representation [:x :o :-
                           :- :- :-
                           :- :- :-])))



(def empty (into [] (repeat 9 :-)))
  

(distinct (sort (map canonical-representation (list empty))))
(distinct (sort (map canonical-representation (for [i (range 9)] (assoc empty i :x)))))
(distinct (sort (map canonical-representation (apply concat (for [i (range 9)] (for [j (range 9)] (assoc (assoc empty i :x) j :o)))))))


(defn addan [x board] 
  (for [i (range 9) :when (= :- (board i))] (assoc board i x)))

(mapcat #(addan :x %) (list empty))

(defn place [x lst]
  (mapcat #(addan x %) lst))

(count (list empty)) ; 1
(count (place :x (list empty))) ; 9
(count (place :o (place :x (list empty)))) ; 72
(count (place :x (place :o (place :x (list empty))))) ; 504
(count (place :o (place :x (place :o (place :x (list empty)))))) ; 3024
(count (place :x (place :o (place :x (place :o (place :x (list empty))))))) ; 15120
(count (place :o (place :x (place :o (place :x (place :o (place :x (list empty)))))))) ; 60480
(count (place :x (place :o (place :x (place :o (place :x (place :o (place :x (list empty))))))))) ; 181440
(count (place :o (place :x (place :o (place :x (place :o (place :x (place :o (place :x (list empty)))))))))) ; 362880
(count (place :x (place :x (place :o (place :x (place :o (place :x (place :o (place :x (list empty)))))))))) ; 362880
(count (place :o (place :x (place :x (place :o (place :x (place :o (place :x (place :o (place :x (list empty))))))))))) ; 362880 ; 362880


(distinct (sort (map canonical-representation (list empty)))) ; ([:- :- :- :- :- :- :- :- :-])
(distinct (sort (map canonical-representation (place :x (list empty))))) ; ([:- :- :- :- :- :- :- :- :x] [:- :- :- :- :- :- :- :x :-] [:- :- :- :- :x :- :- :- :-])

(defn bigaddan [x position-list]
  (distinct (sort (map canonical-representation (place x position-list)))))

(count (bigaddan :x (list empty))) ; 3
(count (bigaddan :o (bigaddan :x (list empty)))) ; 12
(count (bigaddan :x (bigaddan :o (bigaddan :x (list empty))))) ; 38
(count (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (list empty)))))) ; 108
(count (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (list empty))))))) ; 174
(count (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (list empty)))))))) ; 228
(count (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (list empty))))))))) ; 174
(count (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (list empty)))))))))) ; 89
(count (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (list empty))))))))))) ; 23
(count (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (bigaddan :o (bigaddan :x (list empty)))))))))))) ; 0

(reduce + (map count (reductions #(bigaddan %2 %1) (list empty) [:x :o :x :o :x :o :x :o :x]))) ; 850

;; There are 850 distinct positions, up to symmetry.

;; Of course, some are not reachable.




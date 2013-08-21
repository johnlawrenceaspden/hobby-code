(set! *print-length* 103)

(def colonial-troops 225)
(def rebel-troops 126)
(def fronts 7)

(def min-colonial (int (/ 225 7))) ; 32

(def total (* fronts min-colonial))

(def excess (- colonial-troops total))

(def excess-dist (concat (repeat excess 1) (repeat 0)))

(def min-dist (repeat fronts min-colonial))

(def colonial-dist (map + min-dist excess-dist))

(assert (= (reduce + colonial-dist) colonial-troops) "grr")


(defn colonial-troop-allocation[troops fronts]
  (let [min-colonial (quot troops fronts)
        excess (- troops (* min-colonial fronts))
        excess-dist (concat (repeat excess 1) (repeat 0))
        min-dist (repeat fronts min-colonial)
        colonial-dist (map + min-dist excess-dist)]
    (assert (= (reduce + colonial-dist) troops) "grr")
    colonial-dist))

(colonial-troop-allocation 120 1) ;-> (120)
(colonial-troop-allocation 120 2) ;-> (60 60)
(colonial-troop-allocation 120 3) ;-> (40 40 40)

(colonial-troop-allocation 225 3) ;-> (75 75 75)
(colonial-troop-allocation 225 5) ;-> (45 45 45 45 45)
(colonial-troop-allocation 225 7) ;-> (33 32 32 32 32 32 32)
(colonial-troop-allocation 225 8) ;-> (29 28 28 28 28 28 28 28)

(colonial-troop-allocation 7 8) ;-> (1 1 1 1 1 1 1 0)



(defn rebels-allocation [colonial-allocation]
  (let [majority (inc (quot (count colonial-allocation) 2))]
    (map inc (take majority (sort colonial-allocation)))))
        
    
(defn rebellion [troops fronts]
  (let [ca (colonial-troop-allocation troops fronts)
        rn (rebels-allocation ca)]
    (println "colonial allocation" ca)
    (println "rebel allocation" rn)
    (reduce + rn)))

(def doom (map (partial rebellion 225) (range 1 100)))

(filter (fn[[f r]](<= r 126))(map list (drop 1 (range)) doom))















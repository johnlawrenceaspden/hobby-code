;; Rerum Cognoscere Causas

;; A question of epistemology, with reference to the detection of causality, incorporating a slightly smug trick for generating the same random things over and over again.

;; In Al'no'oth, people are rated by their strength (STR) and intelligence (INT)

;; Both are easily determined with simple tests. For instance, a
;; person of STR 11 will win 45% of arm-wrestles against a person of
;; STR 12.

;; The philosophers of Al'no'oth have long suspected that the Gods
;; determine STR and INT by rolling six-sided dice:

(def ^:dynamic *randomizer* (java.util.Random. 0))

(defn rand-int [n]
  (.nextInt *randomizer* n))

(defn D6 [] (inc (rand-int 6)))

;; In fact, the traditional belief is that the Gods use three dice:

(defn three-D6 []
  (reduce + (for [i (range 3)] (D6))))

;; But although all unenhanced humans have scores between 3 and 18 in
;; both characteristics, affirming the suspicions of the philosophers,
;; the higher scores are slightly more common than they should be were
;; 3 dice simply added.

;; Another method the Gods might use would be to roll four dice, and
;; to discard the lowest of them:

(defn three-from-four-D6 []
  (reduce + (drop 1 (sort (for [i (range 4)] (D6))))))


;; At this point, the philosophers disagree. 

;; Those of the first e'dition maintain that all scores are generated using 3 dice.

(defn first-edition [] {:str (three-D6) :int (three-D6)})

;; Those of the second e'dition maintain that, occasionally, by caprice, the Gods use the modified procedure.

(defn mixed []
  (if (zero? (rand-int 10))
    (three-from-four-D6)
    (three-D6)))

(defn second-edition [] {:str (mixed) :int (mixed)})

;; Those of the third e'dition believe that one person in ten is a
;; "player-character".  These characters are awe'some.

;; Awe'someness is not directly detectable, but a player-character has
;; all his characteristics generated using the modified procedure.

(defn third-edition []
  (if (zero? (rand-int 10))
    {:str (three-from-four-D6) :int (three-from-four-D6)}
    {:str (three-D6) :int (three-D6)}))


;; With huge effort, and a very large number of arm-wrestles and
;; riddling contests, a great sage has tabulated the characteristics
;; of an entire village:

(def village
  (binding [*randomizer* (java.util.Random. 0)]
    (doall (repeatedly 100 (case (rand-int 3)
                    0 first-edition 
                    1 second-edition
                    2 third-edition)))))


village ;-> ({:str 13, :int 18} {:str 11, :int 18} {:str 14, :int 15} {:str 6, :int 12} {:str 14, :int 13} {:str 18, :int 10} {:str 15, :int 11} {:str 12, :int 15} {:str 7, :int 8} {:str 16, :int 12} {:str 8, :int 7} {:str 9, :int 14} {:str 10, :int 9} {:str 11, :int 10} {:str 5, :int 10} {:str 7, :int 9} {:str 9, :int 13} {:str 12, :int 9} {:str 13, :int 9} {:str 5, :int 9} {:str 8, :int 13} {:str 9, :int 11} {:str 13, :int 14} {:str 12, :int 14} {:str 12, :int 17} {:str 14, :int 9} {:str 10, :int 11} {:str 18, :int 17} {:str 11, :int 9} {:str 8, :int 9} {:str 15, :int 13} {:str 8, :int 5} {:str 11, :int 9} {:str 10, :int 8} {:str 9, :int 12} {:str 5, :int 11} {:str 10, :int 7} {:str 9, :int 14} {:str 11, :int 9} {:str 11, :int 12} {:str 12, :int 13} {:str 15, :int 9} {:str 12, :int 12} {:str 6, :int 13} {:str 5, :int 4} {:str 12, :int 13} {:str 15, :int 10} {:str 14, :int 14} {:str 11, :int 4} {:str 12, :int 9} {:str 10, :int 12} {:str 7, :int 12} {:str 8, :int 11} {:str 10, :int 10} {:str 9, :int 8} {:str 8, :int 12} {:str 7, :int 9} {:str 13, :int 3} {:str 14, :int 9} {:str 8, :int 9} {:str 10, :int 11} {:str 15, :int 4} {:str 10, :int 11} {:str 8, :int 10} {:str 15, :int 10} {:str 8, :int 13} {:str 12, :int 5} {:str 8, :int 16} {:str 4, :int 8} {:str 10, :int 18} {:str 12, :int 12} {:str 11, :int 10} {:str 12, :int 8} {:str 12, :int 13} {:str 8, :int 12} {:str 9, :int 12} {:str 12, :int 10} {:str 15, :int 10} {:str 8, :int 11} {:str 7, :int 11} {:str 4, :int 8} {:str 12, :int 11} {:str 13, :int 9} {:str 14, :int 13} {:str 5, :int 9} {:str 17, :int 10} {:str 8, :int 13} {:str 9, :int 10} {:str 5, :int 14} {:str 15, :int 12} {:str 13, :int 13} {:str 11, :int 8} {:str 8, :int 6} {:str 12, :int 8} {:str 10, :int 3} {:str 14, :int 9} {:str 15, :int 12} {:str 15, :int 14} {:str 6, :int 10} {:str 16, :int 13})

;; How should the philosophers reason amongst themselves, should their object be to determine the System of their World?




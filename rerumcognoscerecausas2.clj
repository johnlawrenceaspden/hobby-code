;; Rerum Cognoscere Causas II

;; The problem: Which e'dition's rules were used to generate a village?

(def ^:dynamic *randomizer* (java.util.Random. 0))

(defn rand-int [n]
  (.nextInt *randomizer* n))

(defn D6 [] (inc (rand-int 6)))

(defn three-D6 []
  (reduce + (for [i (range 3)] (D6))))

(defn three-from-four-D6 []
  (reduce + (drop 1 (sort (for [i (range 4)] (D6))))))

(defn mixed []
  (if (zero? (rand-int 10))
    (three-from-four-D6)
    (three-D6)))

(defn first-edition [] {:str (three-D6) :int (three-D6)})

(defn second-edition [] {:str (mixed) :int (mixed)})

(defn third-edition []
  (if (zero? (rand-int 10))
    {:str (three-from-four-D6) :int (three-from-four-D6)}
    {:str (three-D6) :int (three-D6)}))

(def village
  (binding [*randomizer* (java.util.Random. 0)]
    (doall (repeatedly 100 (case (rand-int 3)
                    0 first-edition 
                    1 second-edition
                    2 third-edition)))))


village ;-> ({:str 13, :int 18} {:str 11, :int 18} {:str 14, :int 15} {:str 6, :int 12} {:str 14, :int 13} {:str 18, :int 10} {:str 15, :int 11} {:str 12, :int 15} {:str 7, :int 8} {:str 16, :int 12} {:str 8, :int 7} {:str 9, :int 14} {:str 10, :int 9} {:str 11, :int 10} {:str 5, :int 10} {:str 7, :int 9} {:str 9, :int 13} {:str 12, :int 9} {:str 13, :int 9} {:str 5, :int 9} {:str 8, :int 13} {:str 9, :int 11} {:str 13, :int 14} {:str 12, :int 14} {:str 12, :int 17} {:str 14, :int 9} {:str 10, :int 11} {:str 18, :int 17} {:str 11, :int 9} {:str 8, :int 9} {:str 15, :int 13} {:str 8, :int 5} {:str 11, :int 9} {:str 10, :int 8} {:str 9, :int 12} {:str 5, :int 11} {:str 10, :int 7} {:str 9, :int 14} {:str 11, :int 9} {:str 11, :int 12} {:str 12, :int 13} {:str 15, :int 9} {:str 12, :int 12} {:str 6, :int 13} {:str 5, :int 4} {:str 12, :int 13} {:str 15, :int 10} {:str 14, :int 14} {:str 11, :int 4} {:str 12, :int 9} {:str 10, :int 12} {:str 7, :int 12} {:str 8, :int 11} {:str 10, :int 10} {:str 9, :int 8} {:str 8, :int 12} {:str 7, :int 9} {:str 13, :int 3} {:str 14, :int 9} {:str 8, :int 9} {:str 10, :int 11} {:str 15, :int 4} {:str 10, :int 11} {:str 8, :int 10} {:str 15, :int 10} {:str 8, :int 13} {:str 12, :int 5} {:str 8, :int 16} {:str 4, :int 8} {:str 10, :int 18} {:str 12, :int 12} {:str 11, :int 10} {:str 12, :int 8} {:str 12, :int 13} {:str 8, :int 12} {:str 9, :int 12} {:str 12, :int 10} {:str 15, :int 10} {:str 8, :int 11} {:str 7, :int 11} {:str 4, :int 8} {:str 12, :int 11} {:str 13, :int 9} {:str 14, :int 13} {:str 5, :int 9} {:str 17, :int 10} {:str 8, :int 13} {:str 9, :int 10} {:str 5, :int 14} {:str 15, :int 12} {:str 13, :int 13} {:str 11, :int 8} {:str 8, :int 6} {:str 12, :int 8} {:str 10, :int 3} {:str 14, :int 9} {:str 15, :int 12} {:str 15, :int 14} {:str 6, :int 10} {:str 16, :int 13})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The first thing that our philosophers should look at is the distribution of scores according to their models.

;; What do the results of rolling 3D6 look like?
(def threed6f (frequencies 
           (for [i (range 1 7) j (range 1 7) k (range 1 7)] 
             (reduce + [i j k]))))

;; {3 1, 4 3, 5 6, 6 10, 7 15, 8 21, 9 25, 10 27, 11 27, 12 25, 13 21, 14 15, 15 10, 16 6, 17 3, 18 1}

;; What about 4D6 and discarding the lowest?
(def fourd6drop1f (frequencies 
                   (for [i (range 1 7) j (range 1 7) k (range 1 7) l (range 1 7)] 
                     (reduce + (drop 1 (sort [i j k l])))))) 

;; {3 1, 4 4, 5 10, 6 21, 7 38, 8 62, 9 91, 10 122, 11 148, 12 167, 13 172, 14 160, 15 131, 16 94, 17 54, 18 21}

;; So the probability distributions for the dice rolling methods are: 

(defn p3d6 [char] (/ (threed6f char) (reduce + (vals threed6f))))

(defn p4d6drop1 [char] (/ (fourd6drop1f char) (reduce + (vals fourd6drop1f))))

(defn pmixed [char] (+ (* 9/10 (p3d6 char)) (* 1/10 (p4d6drop1 char))))

;; Sanity checks!

;; As every schoolboy once knew:

(p3d6 18) ;-> 1/216
(p3d6 10) ;-> 1/8

(p4d6drop1 18) ;-> 7/432
(p4d6drop1 10) ;-> 61/648

(pmixed 18) ;-> 5/864
(pmixed 3) ;-> 11/2592

; A subtle bias indeed
(/ (pmixed 18) (pmixed 3)) ;-> 15/11

(reduce + (map p4d6drop1 (range 3 19))) ;-> 1N
(reduce + (map p3d6 (range 3 19))) ;-> 1N
(reduce + (map pmixed (range 3 19))) ;-> 1N

;; So the three schools models are:

(defn ptrad [{:keys [str int]}]
  (* (p3d6 int) (p3d6 str)))

(ptrad {:str 18 :int 18}) ;-> 1/46656

(defn pindep [{:keys [str int]}]
  (* (pmixed int) (pmixed str)))

(pindep {:str 18 :int 18}) ;-> 25/746496

(defn pcommon [{:keys [str int]}]
  (+ 
   (* 9/10 (p3d6 int) (p3d6 str))
   (* 1/10 (p4d6drop1 int) (p4d6drop1 str))))

(pcommon {:str 18 :int 18}) ;-> 17/373248

;; My intuition is that very strong, very clever villagers are more likely under the common cause model than under the independent mixed model and even less likely under the traditional model

(< (ptrad {:str 18 :int 18}) (pindep {:str 18 :int 18}) (pcommon {:str 18 :int 18})) ;-> true

(reduce + (map pcommon (for [i (range 3 19) j (range 3 19)] {:int i :str j}))) ;-> 1N
(reduce + (map ptrad   (for [i (range 3 19) j (range 3 19)] {:int i :str j}))) ;-> 1N
(reduce + (map pindep  (for [i (range 3 19) j (range 3 19)] {:int i :str j}))) ;-> 1N


(def ltrad   (reduce * (map ptrad   village)))
(def lcommon (reduce * (map pcommon village)))
(def lindep  (reduce * (map pindep  village)))

(def posteriortrad   (/ ltrad   (+ ltrad lcommon lindep)))
(def posteriorcommon (/ lcommon (+ ltrad lcommon lindep)))
(def posteriorindep  (/ lindep  (+ ltrad lcommon lindep)))

(float posteriortrad) ;-> 0.15591037
(float posteriorcommon) ;-> 0.52879226
(float posteriorindep) ;-> 0.31529734


;; Clearly more research is needed!

(def city
  (binding [*randomizer* (java.util.Random. 0)]
    (doall (repeatedly 1000 (case (rand-int 3)
                    0 first-edition 
                    1 second-edition
                    2 third-edition)))))


(def ltrad   (Math/exp (+ 4970 (reduce + (map #(Math/log %) (map float (map ptrad city))))))))
(def lcommon (Math/exp (+ 4970 (reduce + (map #(Math/log %) (map float (map pcommon city)))))))
(def lindep  (Math/exp (+ 4970 (reduce + (map #(Math/log %) (map float (map pindep city)))))))

(def posteriortrad   (/ ltrad   (+ ltrad lcommon lindep)))
(def posteriorcommon (/ lcommon (+ ltrad lcommon lindep)))
(def posteriorindep  (/ lindep  (+ ltrad lcommon lindep)))

(float posteriortrad) ;-> 0.77889913
(float posteriorcommon) ;-> 0.046136353
(float posteriorindep) ;-> 0.17496453

(def country
  (binding [*randomizer* (java.util.Random. 0)]
    (doall (repeatedly 10000 (case (rand-int 3)
                    0 first-edition 
                    1 second-edition
                    2 third-edition)))))

(def ltrad   (Math/exp (+ 49891 (reduce + (map #(Math/log %) (map float (map ptrad country))))))))
(def lcommon (Math/exp (+ 49891 (reduce + (map #(Math/log %) (map float (map pcommon country)))))))
(def lindep  (Math/exp (+ 49891 (reduce + (map #(Math/log %) (map float (map pindep country)))))))

(def posteriortrad   (/ ltrad   (+ ltrad lcommon lindep)))
(def posteriorcommon (/ lcommon (+ ltrad lcommon lindep)))
(def posteriorindep  (/ lindep  (+ ltrad lcommon lindep)))

(float posteriortrad) ;-> 1.0
(float posteriorcommon) ;-> 3.1192545E-22
(float posteriorindep) ;-> 1.2650255E-19













;; (let [[t c i] (map (fn[pfn] (reduce * (map pfn city))) [ptrad pcommon pindep])
;;       sum (+ t c i)
;;       normalized [(/ t sum) (/ c sum) (/ i sum)]]
;;  (map float normalized))





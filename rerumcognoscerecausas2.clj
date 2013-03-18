;; Rerum Cognoscere Causas II : In Which Divers Probabilities are Deduced Simply by Counting

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

;; What do the results of rolling 3D6 and adding look like? We'll just
;; enumerate the 216 possible ways the dice can fall and count how
;; many ways there are to score the various possible numbers:

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

;; You get 3 sixes only one time in ~200, so 18s are very rare
(p3d6 18) ;-> 1/216
(p3d6 17) ;-> 1/72
;; In fact only one roll in ten is in the 15-18 range:
(reduce + (map p3d6 '(18 17 16 15)))

;; But you see 10 or 11 very often, in fact about a quarter of the time.
(p3d6 10) ;-> 1/8
(p3d6 11) ;-> 1/8

;; However if you get to roll four dice and ignore one of them, 
;; then the high scores are more likely.

(p4d6drop1 18) ;-> 7/432
(p4d6drop1 17) ;-> 1/24
(reduce + (map p4d6drop1 '(18 17 16 15))) ;-> 25/108
;; And in fact you have a quarter chance of an exceptional score 15-18.

;; 10 and 11 are slightly less likely than they were, and 11 is more likely than 10
(p4d6drop1 11) ;-> 37/324
(p4d6drop1 10) ;-> 61/648

;; Truly bad scores are very hard to get
(p4d6drop1 3) ;-> 1/1296
;; In fact only one in six scores are 'below average'.
(reduce + (map p4d6drop1 '(3 4 5 6 7 8 9))) ;-> 227/1296


;; However in the mixed distribution where you might be doing it one
;; way and you might be doing it the other, these differences are not
;; nearly as large:
(pmixed 18) ;-> 5/864
(pmixed 3) ;-> 11/2592

; A subtle bias indeed. 
(/ (pmixed 18) (pmixed 3)) ;-> 15/11

;; Just as a sanity check, check that our three distributions add up to 1.
(reduce + (map p4d6drop1 (range 3 19))) ;-> 1N
(reduce + (map p3d6 (range 3 19))) ;-> 1N
(reduce + (map pmixed (range 3 19))) ;-> 1N


;; Our three schools, with their different beliefs about how characteristics are generated, can now write down the chances
;; of combinations of characteristics:

;; The traditionalists of the first e'dition say that your chance of
;; getting strength 18, intelligence 18 is simply the chance of
;; getting both scores independently.

(defn ptrad [{:keys [str int]}]
  (* (p3d6 int) (p3d6 str)))

;; Being truly gifted is terribly unlikely:
(ptrad {:str 18 :int 18}) ;-> 1/46656 
;; As is being truly disadvantaged:
(ptrad {:str 3  :int  3}) ;-> 1/46656

;; But then, only 1 person in 16 is truly average:
(reduce + (map ptrad (for [s '(10 11) i '(10 11)] {:str s :int i}))) ;-> 1/16

;; Being a weedy genius is just as likely as being a genius who can bend swords.
(ptrad {:str 3 :int 18}) ;-> 1/46656

;; The second e'dition guys still model the two scores as independent things, 
(defn pindep [{:keys [str int]}]
  (* (pmixed int) (pmixed str)))

;; But they predict more beefy einsteins
(pindep {:str 18 :int 18}) ;-> 25/746496

;; And fewer weedy vegetables
(pindep {:str 3 :int 3}) ;-> 121/6718464

;; One good, one bad 
(pindep {:str 3 :int 18}) ;-> 55/2239488
;; Has about the same frequency as in the traditionalists' model
(apply / ((juxt pindep ptrad) {:str 3 :int 18})) ;-> 55/48

;; The third e'dition guys think that you use either the traditional system for both characteristics,
;; or you use the enhanced system for both. They say that there is a common cause, being a 'player character'.
;; And they say that one in ten people are like that.

(defn pcommon [{:keys [str int]}]
  (+ 
   (* 9/10 (p3d6 int) (p3d6 str))
   (* 1/10 (p4d6drop1 int) (p4d6drop1 str))))

;; If we just look at the frequencies of a characteristic in
;; isolation, then we can't tell the difference between the second and
;; third schools. 
;; They make the same guesses about the number of very strong people:

;; The chance of having STR 18 is the chance of having
;; STR 18 and any intelligence
(reduce + (map pcommon (for [i (range 3 19)] {:str 18 :int i}))) ;-> 5/864
(reduce + (map pindep  (for [i (range 3 19)] {:str 18 :int i})))  ;-> 5/864
(reduce + (map ptrad   (for [i (range 3 19)] {:str 18 :int i})))   ;-> 1/216

;; It's only by looking at both scores together that we can see
;; differences between what the world will look like if there's a
;; common cause and what it will look like if there isn't:

;; Both later e'ditions predict more supermen, but the common causers predict even more than the independents:
(map #(int (* 1000000 (float %))) ((juxt pcommon ptrad pindep) {:str 18 :int 18})) ;-> (45 21 33)

;; And they both predict slightly fewer basket cases, but the common causers expect less of a drop.
(map #(int (* 1000000 (float %))) ((juxt pcommon ptrad pindep) {:str 3 :int 3}))   ;-> (19 21 18)

;; The traditionalists predict slightly more cases of people who are good at only one thing
(map #(int (* 1000000 (float %))) ((juxt pcommon ptrad pindep) {:str 18 :int 3}))  ;-> (20 21 24)
(map #(int (* 1000000 (float %))) ((juxt pcommon ptrad pindep) {:str 3 :int 18}))  ;-> (20 21 24)

;; Again, we ought to sanity check our distributions:
(reduce + (map pcommon (for [i (range 3 19) j (range 3 19)] {:int i :str j}))) ;-> 1N
(reduce + (map ptrad   (for [i (range 3 19) j (range 3 19)] {:int i :str j}))) ;-> 1N
(reduce + (map pindep  (for [i (range 3 19) j (range 3 19)] {:int i :str j}))) ;-> 1N

;; Premature optimization is the root of all evil, and there is no
;; place to optimize more premature than an introductory tutorial
;; essay, but I am worried about my transistors wearing out if I have
;; to use these functions a lot.

(def pcommon (memoize pcommon))
(def pmixed  (memoize pmixed))
(def ptrad   (memoize ptrad))

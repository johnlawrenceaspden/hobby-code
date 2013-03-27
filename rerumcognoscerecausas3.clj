;; Rerum Cognoscere Causas III : What can we tell from our small sample?

;; So far we have:

;; Our data, generated at random from a secret algorithm:

(def ^:dynamic *randomizer* (java.util.Random. 0))

(defn rand-int [n] (.nextInt *randomizer* n))

(defn D6 [] (inc (rand-int 6)))

(defn three-D6 [] (reduce + (repeatedly 3 D6)))

(defn three-from-four-D6 [] (reduce + (drop 1 (sort (repeatedly 4 D6)))))

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

;; And the calculations of our sages, who have determined with prodigious effort:
;; the sides on a six sided die
(def r16 (range 1 7))

;; the probabilities of each result for each suggested method of generating a characteristic
(def threed6f (frequencies (for [i r16 j r16 k r16] (reduce + [i j k]))))

(def fourd6drop1f (frequencies (for [i r16 j r16 k r16 l r16]
                     (reduce + (drop 1 (sort [i j k l]))))))

(defn p3d6 [char] (/ (threed6f char) (reduce + (vals threed6f))))

(defn p4d6drop1 [char] (/ (fourd6drop1f char) (reduce + (vals fourd6drop1f))))

(defn pmixed [char] (+ (* 9/10 (p3d6 char)) (* 1/10 (p4d6drop1 char))))

;; And thus the probabilities of a villager with particular characteristics coming into being under their scheme

(def ptrad (memoize (fn [{:keys [str int]}]  (* (p3d6 int) (p3d6 str)))))

(def pindep(memoize (fn [{:keys [str int]}] (* (pmixed int) (pmixed str)))))

(def pcommon (memoize (fn [{:keys [str int]}]
  (+
   (* 9/10 (p3d6 int) (p3d6 str))
   (* 1/10 (p4d6drop1 int) (p4d6drop1 str))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Very strong, very clever villagers are more likely under the common
;; cause model than under the independent mixed model and even less
;; likely under the traditional model:

(apply < ((juxt ptrad pindep pcommon) {:str 18 :int 18})) ;-> true

;; Let us imagine a neutral observer, employed to fairly determine the correctness of the three schools.

;; He starts off looking at each school's argument, and can see no way
;; to decide between the two, so he assigns them odds of 1:1:1 ,
;; meaning that he thinks each one is equally likely, or equivalently,
;; that he will take or place bets on any of the three schools at odds
;; of 2:1 against ( 1 gold piece gets you 2 if you picked right ),
;; (plus a small commission for bookkeeping and accepting risk.)

(def prior [1 1 1])

;; He now considers the first villager:

(def Olaf (first village))

;; Who is strong, and extremely clever:

Olaf ;-> {:str 13, :int 18}

;; And he considers the probability that Olaf would exist under each of the models.

(ptrad Olaf)   ;-> 7/15552
(pindep Olaf)  ;-> 653/1119744
(pcommon Olaf) ;-> 217/349920

;; Having observed some data, he now considers that he should re-weight his beliefs in the three models accordingly:

(map * ((juxt ptrad pindep pcommon) Olaf) prior) ;-> (7/15552 653/1119744 217/349920)

;; A ratio of odds can always be rescaled. 10:5 is the same as 2:1.

(map #(float (* 15552/7 %)) (map * ((juxt ptrad pindep pcommon) Olaf) prior))
;-> (1.0 1.2956349 1.3777778)

;; So here's a function to take any odds ratio and turn it into (approximate) percentages
(defn approx-odds [[a b c]]
  (let [m (/ (+ a b c) 100)]
    (mapv int [(/ a m) (/ b m) (/ c m)])))

(approx-odds (map * ((juxt ptrad pindep pcommon) Olaf) prior)) ;-> [27 35 37]

;; And we can now see that the arbitrator's judgement has shifted a little away from the traditionalists, and towards the second and third e'ditions.

;; This is reasonable, given that he has just observed a man who is both cleverer and stronger than would be expected by the traditionalists

;; Suppose he had seen Magnus instead:
(def Magnus (second village))

Magnus ;-> {:str 11, :int 18}

;; Magnus is average in strength, but very clever. This is probably more likely under the independent rules than it is under the common cause rules.

(approx-odds (map * ((juxt ptrad pindep pcommon) Magnus) prior)) ;-> [28 35 35]

;; Seeing Magnus should change a neutral person's beliefs towards the second e'dition, mostly at the expense of the first.

;; But of course, our observer has seen both:

(approx-odds
 (map * ((juxt ptrad pindep pcommon) Magnus)
      (map * ((juxt ptrad pindep pcommon) Olaf)
           prior))) ;-> [23 37 39]

;; Implying that the combined effect of both men is to discredit the traditional school while slightly favouring the common-cause hypothesis.

;; We could generate the series representing how the assessors beliefs should change as he 
;; considers each villager like this:

(reductions 
 (fn [beliefs villager] (map * ((juxt ptrad pindep pcommon) villager) beliefs))
 prior
 (list Magnus Olaf))
;-> ([1 1 1] (1/1728 803/1119744 247/349920) (7/26873856 524359/1253826625536 53599/122444006400))

;; More readably, we can separate the function which updates our beliefs given a datum.
(defn update [beliefs villager] 
  (map * ((juxt ptrad pindep pcommon) villager) beliefs))

(map approx-odds
 (reductions update prior (list Magnus Olaf)))
;-> ([33 33 33] [28 35 35] [23 37 39])

;; What if we look at the first ten villagers?
(map approx-odds
 (reductions update prior (take 10 village)))
;-> ([33 33 33] [27 35 37] [23 37 39] [19 37 42] [20 37 41] [18 38 43] [16 40 43] [14 41 44] [13 41 45] [14 40 45] [12 40 46])

;; The first twenty:
(approx-odds
 (reduce update prior (take 20 village)))
;-> [19 36 44]

;; And at the whole village?
(approx-odds
 (reduce update prior (take 100 village)))
;;-> [15 31 52]


;; So if we look at our whole village, it looks as though we'd be
;; slightly more confident that we lived in a third e'dition world.

;; But we're really not terribly confident about that.
;; We know that the models and priors are spot on, since
;; we've seen the source code for the world.

;; But even then, if we declared on the basis of this one village that
;; we lived in a third edition world, we'd literally expect to be
;; wrong half the time.  

;; That's only a slight improvement on being wrong two thirds of the
;; time, which we'd expect if we hadn't bothered to look at any data at
;; all. Most of our opinion is coming from our prior beliefs and there
;; is really very little evidence in the data that we have.

;; Clearly more research is needed!


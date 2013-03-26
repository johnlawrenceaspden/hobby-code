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


;; As it happens, our village is just one of ten in the district
(def district
  (binding [*randomizer* (java.util.Random. 0)]
    (doall (repeatedly 1000 (case (rand-int 3)
                    0 first-edition
                    1 second-edition
                    2 third-edition)))))

(def villages (partition 100 district))

;; paranoid check
(= village (first villages)) ;-> true

;; So let's see what conclusions we can draw from each individual village

(for [v villages] 
  (approx-odds
   (reduce 
    (fn [beliefs villager] (map * ((juxt ptrad pindep pcommon) villager) beliefs))
    prior v)))
;-> ([15 31 52] [53 29 17] [29 38 32] [63 21 14] [34 37 27] [36 27 36] [39 38 22] [49 30 19] [48 24 27] [20 37 42])

;; Some villages are pointing one way, and some the other. 

;; We might want to consider the district as a whole:
(approx-odds
 (reduce update prior district))

;; Unfortunately this expression takes a while to evaluate. Can you see why?

;; Here's a clue:
(reduce update [1.0 1.0 1.0] (take 100 district)) ;-> (1.0019121633199549E-224 2.0261656590064221E-224 3.398128024413593E-224)
;; And rather more worryingly
(reduce update [1.0 1.0 1.0] district) ;-> (0.0 0.0 0.0)

;; It looks as though mathematics itself is failing us!

;; We need to find a new way to multiply numbers.

;; Fortunately we can add their logarithms instead:

(+ (Math/log 6) (Math/log 6)) ;-> 3.58351893845611
(Math/log (* 6 6)) ;-> 3.58351893845611

;; So let's make a log version of our update function
(defn log-update [beliefs villager] 
  (map + (map #(Math/log %) ((juxt ptrad pindep pcommon) villager)) beliefs))

;; And of our prior:
(def log-prior (map #(Math/log %) prior))

;; And try that on the village
(reduce log-update log-prior village)
;; (-515.7771504932035 -515.0729156616442 -514.5558361317242)

;; If we know the logs, we can get the numbers we actually want back:
(map #(Math/exp %) (reduce log-update log-prior village)) 
;-> (1.0019121633198053E-224 2.0261656590065323E-224 3.398128024414225E-224)
(approx-odds (map #(Math/exp %) (reduce log-update log-prior village))) 
;-> [15 31 52]

;; So we've found a sneaky way to do the multiplications without
;; running off the bottom of the floating point range.

(reduce log-update log-prior district)
;;-> (-4967.368738149676 -4968.862029975447 -4970.195021140233)

;; That's done our probability calculation for us, but unfortunately we can't actually recover
;; the number that -4967.36... is the logarithm of
(Math/exp -4967)

;; So we need to pull another rabbit out of our hat. 

;; All we're interested in is the ratios of the three likelihoods.

;; And ratios aren't affected by multiplying through

(approx-odds [2 4 8]) ;-> [14 28 57]
(approx-odds [6 12 24]) ;-> [14 28 57]

;; Equivalently, "ratios" of logs aren't affected by adding a constant.

(map #(Math/log %) [2 4 8]) ;-> (0.6931471805599453 1.3862943611198906 2.0794415416798357)
(map #(Math/log %) [6 12 24]) ;-> (1.791759469228055 2.4849066497880004 3.1780538303479458)

;; Multiplying the numbers by a constant has exactly the same effect as adding a constant to each log:
(map - 
     (map #(Math/log %) [6 12 24]) 
     (map #(Math/log %) [2 4 8])) ;-> (1.0986122886681096 1.0986122886681098 1.09861228866811)


;; So what we'd like to be able to calculate is what this would be:
(approx-odds (map #(Math/exp %)
                  '(-4967.368738149676 -4968.862029975447 -4970.195021140233)))
;; if only floating point arithmetic was good enough.


;; So we'll calculate instead:
(approx-odds (map #(Math/exp %)
                  '(-0.368738149676 -1.862029975447 -3.195021140233)))

;-> [77 17 4]


;; So after looking at a full thousand people, with perfect models and
;; a perfect prior, looking for what you would have thought was a
;; pretty obvious effect, the existence of gifted superbeings, we're
;; still in a bit of a dubious position.

;; If we decide 'that's good enough', and declare that we live in a
;; first e'dition world, then we've still got a fair chance of being
;; wrong.


(defn log-update [beliefs villager] 
  (doall (map + (map #(Math/log %) ((juxt ptrad pindep pcommon) villager)) beliefs)))

(defn posterior [prior data]
  (reduce log-update (map #(Math/log %) prior) data))

(defn percentages-from-log-beliefs [beliefs]
  (let [bmax (apply max beliefs)
        odds (for [b beliefs] (Math/exp (- b bmax)))]
    (approx-odds odds)))

;; paranoid checking again:
(percentages-from-log-beliefs (posterior [1 1 1] village)) ;-> [15 31 52]
(percentages-from-log-beliefs (posterior [1 1 1] district)) ;-> [77 17 4]

;; Another way to look at this is to examine the reactions of the three schools to the data

;; Suppose the first e'dition philosophers start their analysis with a
;; prior [10 1 1], confident enough in their own arguments to bet on
;; them at 5:1, but grudgingly admitting the possibility that one of
;; the other schools might be right.
(percentages-from-log-beliefs (posterior [10 1 1] district)) ;-> [97 2 0]

;; Not unreasonably, after looking at the data, they're now very confident they were right all along.    

;; The second e'dition guys, however, are troubled, but only to the point where they feel unsure.
(percentages-from-log-beliefs (posterior [1 10 1] district)) ; [30 67 1]

;; But the third e'dition, starting off believing in common cause,
;; should actually change their minds if they examine the data fairly, and go over to the first camp.
(percentages-from-log-beliefs (posterior [1 1 10] district)) ; [55 12 32]


;; Obviously, we could look at an even larger sample:
(def country
  (binding [*randomizer* (java.util.Random. 2000)]
    (doall (repeatedly 10000 (case (rand-int 3)
                               0 first-edition
                               1 second-edition
                               2 third-edition)))))

(percentages-from-log-beliefs (posterior [1 1 1] (take 10000 country))) ;-> [27 47 25]
(percentages-from-log-beliefs (posterior [1 1 1] (take 1000 (drop 1000 country)))) ;-> [0 90 9]
(percentages-from-log-beliefs (posterior [1 1 1] (take 1000 (drop 2000 country)))) ;-> [0 54 45]
(percentages-from-log-beliefs (posterior [1 1 1] (take 1000 (drop 3000 country)))) ;-> [1 60 38]
(percentages-from-log-beliefs (posterior [1 1 1] (take 1000 (drop 4000 country)))) ;-> [0 77 22]
(percentages-from-log-beliefs (posterior [1 1 1] (take 1000 (drop 5000 country)))) ;-> [10 64 24]
(percentages-from-log-beliefs (posterior [1 1 1] (take 1000 (drop 6000 country)))) ;-> [18 49 31]
(percentages-from-log-beliefs (posterior [1 1 1] (take 1000 (drop 7000 country)))) ;-> [0 64 35]
(percentages-from-log-beliefs (posterior [1 1 1] (take 1000 (drop 8000 country)))) ;-> [31 47 20]
(percentages-from-log-beliefs (posterior [1 1 1] (take 1000 (drop 9000 country)))) ;-> [20 68 10]





(reduce log-update [0.0 0.0 0.0] (take 1000 country))
(-4967.368738149676 -4968.862029975447 -4970.195021140233)
(reduce log-update [0.0 0.0 0.0] (take 1030 country))
(reduce log-update [0.0 0.0 0.0] (take 1035 country))
(reduce log-update [0.0 0.0 0.0] (take 1036 country))
(reduce log-update [0.0 0.0 0.0] (take 1037 country))
(reduce log-update [0.0 0.0 0.0] (take 1040 country))
(reduce log-update [0.0 0.0 0.0] (take 1050 country))

(defn log-update [beliefs villager] 
  (map + (map #(Math/log %) [1/10 1/100 1/1000]) beliefs))

(reduce log-update [1 1 1] (range 10000))

(def doom (map #(Math/log %) [1/10 1/100 1/1000]))

(defn log-update [beliefs villager] 
  (map + doom beliefs))

(reduce log-update [1 1 1] (range 1038))
(reduce log-update [1 1 1] (range 1039))

(def doom '(1 2 3))

(defn log-update [beliefs villager] 
  (map + doom beliefs))

(reduce log-update [1 1 1] (range 1037))
(reduce log-update [1 1 1] (range 1038))


;;;;;;;;;;;;; a bug, I believe:

(defn log-update [beliefs villager] 
  (doall (map + '(1 2 3) beliefs)))

(reduce log-update [1 1 1] (range 1037))
(reduce log-update [1 1 1] (range 1038))









(reduce log-update '(-4967.368738149676 -4968.862029975447 -4970.195021140233) (take 1000 (drop 1000 country)))




























(approx-odds (map * [15 31 52] [53 29 17] [29 38 32] [63 21 14] [34 37 27] [36 27 36] [39 38 22] [49 30 19] [48 24 27] [20 37 42]))
;-> [78 17 4]




(map approx-odds
 (map (fn[vs] (reduce (fn [beliefs villager] (map * ((juxt ptrad pindep pcommon) villager) beliefs)) [1.0 1.0 1.0] vs)) (partition 100 country))
)

(approx-odds (map * [15N 31N 52N] [53 29 17] [29 38 32] [63 21 14] [34 37 27] [36 27 36] [39 38 22] [49 30 19] [48 24 27] [20 37 42] [28 32 38] [37 33 29] [31 42 26] [52 30 17] [73 14 12] [64 17 17] [29 34 36] [40 34 25] [47 31 20] [52 24 23] [36 37 25] [54 23 21] [72 15 11] [29 39 31] [79 10 10] [60 20 19] [24 43 32] [41 31 27] [51 19 29] [28 35 36] [17 45 37] [77 9 13] [64 15 20] [49 28 22] [27 36 36] [15 45 39] [64 17 17] [30 33 35] [24 51 23] [59 22 17] [73 13 12] [37 28 34] [54 24 20] [47 28 23] [58 22 19] [72 11 16] [22 33 43] [60 20 19] [55 24 20] [52 31 15] [11 52 36] [42 24 32] [50 23 26] [37 28 33] [78 10 10] [25 50 24] [48 25 26] [47 26 25] [28 35 36] [46 26 26] [67 18 14] [55 26 17] [7 33 58] [24 33 42] [58 23 17] [68 16 15] [63 19 17] [50 24 25] [38 34 26] [59 17 23] [69 12 18] [66 16 17] [31 35 32] [19 39 40] [10 43 46] [31 29 38] [47 27 24] [55 25 19] [72 15 12] [26 40 33] [41 32 26] [57 22 19] [66 14 18] [37 31 31] [44 24 30] [41 27 31] [65 17 17] [54 19 26] [20 35 43] [60 19 19] [30 39 30] [65 17 16] [27 39 33] [21 30 47] [82 8 8] [41 30 28] [35 39 24] [67 18 14] [23 36 40] [5 35 59]))







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

(def lltrad   (reduce + (map #(Math/log %) (map float (map ptrad country)))))
(def llcommon (reduce + (map #(Math/log %) (map float (map pcommon country)))))
(def llindep  (reduce + (map #(Math/log %) (map float (map pindep country)))))

lltrad ; -49891.65823367781
llcommon ; -49941.17751169551
llindep ; -49935.172258139675

(def ltrad   (Math/exp (+ 49800 lltrad)))
(def lcommon (Math/exp (+ 49800 llcommon)))
(def lindep  (Math/exp (+ 49800 llindep)))

(def posteriortrad   (/ ltrad   (+ ltrad lcommon lindep)))
(def posteriorcommon (/ lcommon (+ ltrad lcommon lindep)))
(def posteriorindep  (/ lindep  (+ ltrad lcommon lindep)))

(float posteriortrad) ;-> 1.0
(float posteriorcommon) ;-> 3.1192545E-22
(float posteriorindep) ;-> 1.2650255E-19














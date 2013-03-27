;; Rerum Cognoscere Causas IV : How to Deal with Larger Samples.

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


;; using these calculations, we can take beliefs
(def prior [1 1 1])

;; and update them when we find new evidence.
(defn update [beliefs villager] 
  (map * ((juxt ptrad pindep pcommon) villager) beliefs))

;; since ratios are a bit unreadable, this function will allow us to render them as (approximate) percentages.
(defn approx-odds [[a b c]]
  (let [m (/ (+ a b c) 100)]
    (mapv int [(/ a m) (/ b m) (/ c m)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Earlier we found that there's not really enough information in our village to convince anyone one way or the other.
(approx-odds (reduce update prior village)) ;-> [15 31 52]

;; It points half-heartedly towards a common cause model, but we wouldn't be surprised if that was the wrong answer.

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
  (approx-odds (reduce update prior v)))
;-> ([15 31 52] [53 29 17] [29 38 32] [63 21 14] [34 37 27] [36 27 36] [39 38 22] [49 30 19] [48 24 27] [20 37 42])

;; Some villages are pointing one way, and some the other. 

;; We might want to consider the district as a whole:
;; (approx-odds (reduce update prior district))

;; But unfortunately this expression takes a while to evaluate. Can you see why?

;; Here's a clue:
(reduce update [1.0 1.0 1.0] (take 100 district)) ;-> (1.0019121633199549E-224 2.0261656590064221E-224 3.398128024413593E-224)
;; And rather more worryingly
(reduce update [1.0 1.0 1.0] district) ;-> (0.0 0.0 0.0)

;; It looks as though mathematics itself is failing us!

;; We need to find a new way to multiply numbers.

;; Fortunately we can add their logarithms instead:

(+ (Math/log 6) (Math/log 6)) ;-> 3.58351893845611
(Math/log (* 6 6)) ;-> 3.58351893845611

;; So let's make a logarithmic version of our update function
(defn log-update [beliefs villager] 
  (map + (map #(Math/log %) ((juxt ptrad pindep pcommon) villager)) beliefs))

;; And of our prior:
(def log-prior (map #(Math/log %) prior))

;; And try that on the village
(reduce log-update log-prior village)
;; (-515.7771504932035 -515.0729156616442 -514.5558361317242)

;; If we know the logs, we can get the numbers we actually want:
(map #(Math/exp %) (reduce log-update log-prior village)) 
;-> (1.0019121633198053E-224 2.0261656590065323E-224 3.398128024414225E-224)
;; Notice that these numbers are not exactly the same as the numbers calculated above. 

;; Exercise for the reader: which of the two answers we can calculate is closer to the answer we would like to calculate but can't?

;; We can then turn those numbers into percentages
(approx-odds (map #(Math/exp %) (reduce log-update log-prior village))) 
;-> [15 31 52]

;; So we can do our calculation on the district as a whole:

(reduce log-update log-prior district)
;;-> (-4967.368738149676 -4968.862029975447 -4970.195021140233)

;; Hooray, now we have the logarithms of the numbers we really want!

;; That's done our probability calculation for us, but unfortunately we can't actually recover
;; the number that -4967.36... is the logarithm of:
(Math/exp -4967)
;; Because it's too small to fit into floating point arithmetic.

;; So we need to pull another rabbit out of our hat:

;; All we're interested in is the ratios of the three likelihoods.

;; That's not affected by multiplying through by a constant
(approx-odds [2 1 1]) ; [50 25 25] 
(approx-odds [6 3 3]) ; [50 25 25] 
(approx-odds [1332 666 666]) ; [50 25 25]

;; Similarly, if the likelihoods are expressed as logarithms, the odds ratio
;; isn't affected by adding a constant.
(approx-odds (map #(Math/exp %) [2 1 1])) ; [57 21 21]
(approx-odds (map #(Math/exp %) [3 2 2])) ; [57 21 21]
(approx-odds (map #(Math/exp %) [4 3 3])) ; [57 21 21]

;; What we'd like to be able to calculate is what this would be:
;; (approx-odds (map #(Math/exp %) '(-4967.368738149676 -4968.862029975447 -4970.195021140233)))

;; But we can't, since the Math/exp function is broken on numbers this low.


;; So we'll calculate instead (adding 4967 to each log-likelihood):
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

;; We can generalize our procedure thus:

(defn log-update [beliefs villager] 
  (doall (map + (map #(Math/log %) ((juxt ptrad pindep pcommon) villager)) beliefs))) ; the doall is papering over a bug in clojure's lazy sequences

(defn log-posterior [log-prior data]
  (reduce log-update log-prior data))

(defn log-prior [& s]
  (map #(Math/log %) s))

(defn percentages-from-log-beliefs [beliefs]
  (let [bmax (apply max beliefs)
        odds (for [b beliefs] (Math/exp (- b bmax)))]
    (approx-odds odds)))

;; paranoid checking again:
(percentages-from-log-beliefs (log-posterior (log-prior 1 1 1) village)) ;-> [15 31 52]
(percentages-from-log-beliefs (log-posterior (log-prior 1 1 1) district)) ;-> [77 17 4]


;; And look at an even larger sample:
(def country
  (binding [*randomizer* (java.util.Random. 0)]
    (doall (repeatedly 10000 (case (rand-int 3)
                               0 first-edition
                               1 second-edition
                               2 third-edition)))))

(percentages-from-log-beliefs (log-posterior (log-prior 1 1 1) (take 100 country))) ; [15 31 52]
(percentages-from-log-beliefs (log-posterior (log-prior 1 1 1) (take 300 country))) ; [27 39 32]
(percentages-from-log-beliefs (log-posterior (log-prior 1 1 1) (take 1000 country))) ; [77 17 4]
(percentages-from-log-beliefs (log-posterior (log-prior 1 1 1) (take 1500 country))) ; [96 3 0]

(percentages-from-log-beliefs (log-posterior (log-prior 1 1 1) (take 3000 country))) ; [99 0 0]

;; Finally, this looks decisive!
;; And everyone agrees. (Their prior beliefs are *overwhelmed* by the evidence)
(percentages-from-log-beliefs (log-posterior (log-prior 10 1 1) (take 3000 country))) ; [99 0 0]
(percentages-from-log-beliefs (log-posterior (log-prior 1 10 1) (take 3000 country))) ; [99 0 0]
(percentages-from-log-beliefs (log-posterior (log-prior 1 1 10) (take 3000 country))) ; [99 0 0]

;; If we're completely paranoid:
(percentages-from-log-beliefs (log-posterior (log-prior 1 1 1) (take 10000 country))) ; [100 0 0]

;; That's close enough for government work.





;; So, in summary, we're looking at a situation where we understand perfectly what's going on, but can't directly see which of the three alternatives

;; (a) everyone's on a bell curve
;; (b) it's not quite a bell curve, it's a bit biased towards good scores
;; (c) there is a sub-race of gifted superbeings

;; was chosen by our Dungeon Master. We just have to look at characteristic scores.

;; And all our measurements are perfect, there is no noise in the data at all, and no systematic bias in our sampling.

;; And we are using, as far as I know, platonically ideal statistical
;; methods perfectly suited to the problem in hand, and guaranteed to
;; extract absolutely all the significance from our data that there
;; is.

;; And still, we need one thousand data points to get a feel for which way the wind is blowing (one hundred was actively misleading!)
;; And rather more than that to make people change their minds.

;; I would be most interested to know if anyone thinks I've done this analysis wrongly. 
;; Things I am not quite certain of are the randomness of the random number generator and the behaviour of the floating point numbers.
;; I'm reasonably confident of the basic method of comparing the three models, and so that means I'll be even more interested and grateful
;; if someone can show me that I'm talking rubbish.

;; If it's true, it makes me wonder how it is possible to know anything at all about anything interesting.

;; Next time I see a study claiming to show something, I might see if I can make this sort of analysis work on it.
;; Suppose we have a biased 6 sided die, and all we know about it is that it has
;; an average roll of 2.5.

;; We can model one such die so:

(def die [0 0.5 0.5 0 0 0]) ; that is, it comes up 2 half the time and three half the time.

;; as a sanity check, the probabilities should sum to one

(defn sum [v] (reduce + v))

(sum die) ; 1.0

;; The average score should be 2.5

(defn average [v] (reduce + (map * v (range 1 7))))

(average die) ; 2.5

;; There are a lot of possible dice that can satisfy these two constraints.

(def dice'((0 0.5 0.5 0 0 0)
           (0.25 0.25 0.25 0.25 0 0)
           (0.35 0.15 0.15 0.35 0 0)
           (0.35 0.15 0.25 0.15 0.10 0)
           ))

;; sanity checks for all of them:
(map #(vector (sum %) (average %)) dice)

;; There's another calculation we can perform on a probability distribution.
;; The entropy is a measure of the average information provided by an event.

(defn entropy [v] (reduce - 0 (map #(if (zero? %) 0 (* % (Math/log %))) v)))

(map entropy dice) ; (0.6931471805599453 1.3862943611198906 1.3040114826148388 1.5134058386196787)

;; Under this measure, the later dice are 'more random' than the earlier ones on the list.

;; What do we get if we look for the most random die?

;; To make another die with the same sum and average, I can take a little bit of probability from one place, split it in half, and move a bit up and a bit down.

;; Start with a die like this: ( 0 0.5 0.5 0 0 0 )

;; Decide to move some probability from the third element.

;; Choose an amount (0.1), halve it (2 x 0.5), move one half up and one half down:

;; ( 0 0.6 0.4 0.1 0 0 )

;; Here's a function to do that at random


(defn perturb [v]
  (let [i (inc (rand-int (dec (dec (count v)))))
        amount (* (- 0.5 (rand)) (nth v i))
        half-amount (/ amount 2)
        new-i (- (nth v i) amount)
        new-deci (+ (nth v (dec i)) half-amount)
        new-inci (+ (nth v (inc i)) half-amount)]
    (if (and (> new-i 0) (> new-inci 0) (> new-deci 0))
      (-> v 
          (assoc (inc i) new-inci)
          (assoc i new-i)
          (assoc (dec i) new-deci))
      v)))

(def dice (iterate perturb die))

(map sum dice)
(map average dice)
(map entropy dice)

(use '[clojure.pprint :as cpp])

(cpp/cl-format nil "~{~$,~}" (take 100 (drop 100 (map entropy dice))))

(defn increase-entropy [v]
  (let [pv (perturb v)]
    (if (> (entropy pv) (entropy v))
      pv
      v)))

(def dice (iterate increase-entropy die))

(cpp/cl-format nil "~{~5$,~}" (take 100 (drop 100 (map entropy dice))))

(nth dice 200) ; [0.30927144341093 0.2636275430776025 0.19274395572262332 0.12276714821018929 0.07536644704669124 0.03622346253196358]

(nth dice 1000) ; [0.33760745060776626 0.2450459541376965 0.17294208304279676 0.11745788801959382 0.07803690524280471 0.04890971894934193]

(nth dice 10000) ; [0.3463183078498813 0.24048101791661436 0.16625130285496875 0.1146273308244395 0.0784758188703508 0.0538462216837453]

(nth dice 100000) ; [0.34739491516509474 0.2398315647399297 0.16551351017512422 0.11419410304034337 0.07877042860874835 0.05429547827075968]


;; This looks awfully like a decaying exponential
(map /
     (nth dice 100000)
     (map #(Math/exp (- %)) (map (partial * 0.371) (range 6))))

(* (Math/exp 0.371)
   (Math/exp 0.371))















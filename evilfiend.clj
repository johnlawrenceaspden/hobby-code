;; There is a classic maths problem known as 100 prisoners, 100 boxes

;; The first mention of it that I know of is:
;; http://www.springerlink.com/content/c1107q6614555085/

;; I have attempted to describe it here:
;; http://johnlawrenceaspden.blogspot.com/2010/09/prisoners-dilemma.html

;; Without loss of generality, let us assume that the prisoners' names are 0,1,2,3,...n
;; where n is 99 for the problem as stated.

;; We'll represent today's permutation of the cards in the boxes as a vector
;; where the number at position i is the number of the card in box i

;; So, for n = 10, [ 8 3 2 6 0 7 1 9 4 5 ] will do as an example arrangement
;; Card 8 is in box 0, Card 3 is in box 1, etc.

;; The prisoner's optimal strategy is to go to a box, read the number on the card in the box,
;; and then go to that box.

;; Each prisoner should start at his own box, and repeat this strategy
;; until he has run out of moves or found his card.

;; Given our permutation, let us make a function which performs this step:
(defn iterator [boxes]
  (vec boxes))

;; Here is a prisoner going to box 0 and being redirected to box 8
;; ((iterator [ 8 3 2 6 0 7 1 9 4 5]) 0) -> 8

;; We can model the repetition of this process like so
(defn iterations [boxes n]
  (iterate (iterator boxes) n))

;; Here is prisoner 0, going to box 0, then 8, then 4, where he finds his card 0 and wins
;; After that, if he carried on, the cycle would repeat. We'll just look at the first 10 moves:
;; (take 10 (iterations [ 8 3 2 6 0 7 1 9 4 5] 0)) ->  (0 8 4 0 8 4 0 8 4 0)
;; Here is prisoner 1:
;; (take 10 (iterations [ 8 3 2 6 0 7 1 9 4 5] 1)) ->  (1 3 6 1 3 6 1 3 6 1)
;; And prisoner 2, whose card is in his own box:
;; (take 10 (iterations [ 8 3 2 6 0 7 1 9 4 5] 2)) ->  (2 2 2 2 2 2 2 2 2 2)
;; Prisoner 3 follows a similar path to prisoner 1
;; (take 10 (iterations [ 8 3 2 6 0 7 1 9 4 5] 3)) ->  (3 6 1 3 6 1 3 6 1 3)
;; Prisoner 4 follows a similar path to prisoner 0
;; Here is prisoner 5
;; (take 10 (iterations [ 8 3 2 6 0 7 1 9 4 5] 5)) ->  (5 7 9 5 7 9 5 7 9 5)
;; All the prisoners follow one of these four types of paths around the postroom, although
;; they each start at different places on the paths.

;; We'll call 0 8 and 4 the 'orbit' of prisoner 0 with this arrangement of cards.
(defn orbit [boxes n]
  (set (take (count boxes) (iterations boxes n))))

;; Here is prisoner 0's orbit
;; (orbit [ 8 3 2 6 0 7 1 9 4 5] 0) -> #{0 4 8}
;; And here is prisoner 5's
;; (orbit [ 8 3 2 6 0 7 1 9 4 5] 5) -> #{5 7 9}

;; The orbits split the boxes into groups. If you're at 0, 8, or 4, then you never get anywhere
;; else, and nobody who isn't at 0, 8, or 4 is ever going to find card 0, 8, or 4 in their box.
;; Likewise for all the other orbits that people may be following with this arrangement of cards.
(defn partitions [boxes]
  (set (map #(orbit boxes %) (range (count boxes)))))

;; For the example arrangement, the partitions are
;; (2 -> 2 )
;; (0 -> 8 -> 4 -> 0)
;; (5 ->  7 ->  9 -> 5) and
;; (1 3 6)
;; (partitions [ 8 3 2 6 0 7 1 9 4 5]) -> #{#{2} #{1 3 6} #{0 4 8} #{5 7 9}}

;; Now, each prisoner has to go all the way round his orbit to find his card.
;; We'll call the sizes of the orbits the signature of the arrangement.
(defn signature [boxes]
  (sort (map count (partitions boxes))))

;; The signature of our example arrangement is
;; (signature [ 8 3 2 6 0 7 1 9 4 5]) -> (1 3 3 3)

;; One lucky prisoner will find his card immediately, and all the others will need to open three boxes.

;; We can see some other signatures by using random shuffles as input
;; (signature (shuffle (range 10)))

;; We're only interested in the worst case. The prisoners who take the longest to find their cards:
(defn largest-cycle [boxes]
  (apply max (signature boxes)))

;; We know that that's three for our example problem
;; (largest-cycle [ 8 3 2 6 0 7 1 9 4 5]) -> 3

;; We'll generate some sequences by repeated shuffling of the cards.
;; Let's say that (given n prisoners, n boxes, and n cards), the guards
;; shuffle the cards 10 times, and then every day shuffle them again.
(defn boxes-seq[n] (drop 10 (iterate shuffle (range n))))

;; The interesting thing about these permutations, from the prisoner's point of view,
;; is the length of the largest cycle. If it's short, they win. If it's long, they lose.
(defn largest-cycles[boxes-seq] (map largest-cycle boxes-seq))

;; We can generate 31 shuffles of 100 cards, as in the problem:
;; Here is a list of the cycle lengths in a random example:
;; (take 31 (largest-cycles (boxes-seq 100)))
;; (99 50 72 52 55 84 36 79 55 57 81 48 63 98 80 57 69 94 90 87 97 77 69 52 43 50 93 62 94 37 83)

;; In this case, on the first day, one prisoner will find his card in his own box, but the other 99
;; would have taken 99 goes each to find theirs! As soon as the first one goes over the fifty limit,
;; the game is over.

;; But the prisoners will win on the second day, because the largest cycle length is 50 exactly.
;; Fifty of them will find their cards in the fiftieth box.
;; Everyone else will find theirs before fifty tries.

;; On the seventh day, everyone will find their cards within 36 tries.

;; We can do a monte-carlo simulation to find, approximately, the odds of winning on any given day.

;; Let's have a look at a typical random sequence:
;; (take 100 (largest-cycles (boxes-seq 100))) -> (65 52 54 75 87 58 40 45 79 71 61 67 46 49 98 62 79 30 90 42 95 65 46 62 60 88 68 82 90 46 45 42 70 66 76 58 51 96 77 69 90 83 88 75 67 72 68 54 99 64 38 49 42 97 73 38 82 58 56 54 24 32 46 66 53 99 54 90 43 51 53 53 93 37 47 35 39 52 77 35 36 40 76 89 52 39 85 68 46 60 40 41 93 85 41 89 43 42 81 65)

;; Let's define a sequence of the frequencies of each maximum cycle length, just by counting up
;; the first few numbers in this sequence:
(defn cumulative-frequency-sequence [largest-cycles-sequence]
  (map (fn[m] (frequencies (take m largest-cycles-sequence))) (iterate inc 1)))


(take 5 (cumulative-frequency-sequence (largest-cycles (boxes-seq 100))))

;; (sort (nth (freqs (largest-cycles (boxes-seq 100))) 10)) -> ([40 1] [45 1] [52 1] [54 1] [58 1] [61 1] [65 1] [71 1] [75 1] [79 1] [87 1])
;; After 11 trials, there was one where the maximal cycle was 40, and one where it was 45.
;; On the other 9 days, the largest cycle was too long, and the airmen lost the game.

;; After 101 trials, there was one where the largest cycle was 24, one where it was 30, etc
;; (sort (nth (freqs (largest-cycles (boxes-seq 100))) 100)) -> ([24 1] [30 1] [32 1] ......

;; Given a number of trials and a threshold, we can split them into wins and losses:

(defn wins-losses [cumulative-frequency threshold]
  (let [losses (filter (fn[[k v]] (<  threshold k)) cumulative-frequency)
        wins   (filter (fn[[k v]] (>= threshold k)) cumulative-frequency)
        count-up  (fn [m] (apply + (map second m)))]
    [(count-up wins), (count-up losses)]))

(defn wins-losses-seq [threshold cumulative-frequency-seq]
  (map (fn [cf] (wins-losses cf threshold)) cumulative-frequency-seq))

(take 5 (wins-losses-seq 50 (cumulative-frequency-sequence (largest-cycles (boxes-seq 100)))))

(defn wins-and-losses-ratio-seq [wins-losses-seq]
  (map (fn[[w l]] (/ w (+ w l))) wins-losses-seq))


(take 20 (wins-and-losses-ratio-seq (wins-losses-seq 50 (cumulative-frequency-sequence (largest-cycles (boxes-seq 100))))))


(def trial-seq (map #(* 1.0 %) (wins-and-losses-ratio-seq (wins-losses-seq 50 (cumulative-frequency-sequence (largest-cycles (boxes-seq 100)))))))

;; Here are the results of some trials
(comment
  (nth trial-seq 10)   ;; 0.1818181818181818
  (nth trial-seq 20)   ;; 0.2857142857142857
  (nth trial-seq 40)   ;; 0.2439024390243902
  (nth trial-seq 80)   ;; 0.2962962962962963
  (nth trial-seq 160)  ;; 0.3478260869565217
  (nth trial-seq 320)  ;; 0.3457943925233645
  (nth trial-seq 640)  ;; 0.3042121684867395
  (nth trial-seq 1280) ;; 0.3013270882123341
  (nth trial-seq 2560) ;; 0.3053494728621632
  (nth trial-seq 5120) ;; 0.3057996485061511
  (nth trial-seq 10240) 
  )

;; The wins-losses ratio seems to have settled down to about 30%

;; The chances of the airmen losing every game are therefore something like 70% to the power of 30

(Math/pow 0.7 30) ;; .000022539340290692213

;; It seems that they are, in fact, very unlikely to lose as long as they find the right strategy.
;; Good old Dr EvilFiend! He is clearly a softie at heart.

;;-------------------------------------------------------------------------------------------------------------------

;; Although it's given us a plausible sounding answer, the program seems a little slow to run.

;; Let's time some of the parts

(defn profile [n]
  (println "start")
  (let [a (time (doall (take n (boxes-seq 100))))
        b (time (doall (take n (largest-cycles a))))
        c (time (doall (take n (cumulative-frequency-sequence b))))
        d (time (doall (take n (wins-losses-seq 50 c))))
        e (time (doall (take n (map #(* 1.0 %) (wins-and-losses-ratio-seq d)))))]
    (println "done")
    (map (vec e) '(1 2 4 8 16 32 64))))

(profile 5120)

;;My results were:
;;10 msecs to generate 100 permutations of 100 numbers
;;2900 msecs to find the largest cycles
;;20 msecs to calculate the cumulative frequencies
;;40 msecs to count wins and losses
;;5  msec  to turn those into ratios

;;Clearly the first thing to look at is the largest-cycles function

(defn profile [n]
  (println "start")
  (let [a (time (doall (take n (boxes-seq  100))))
        b (time (doall (take n (map partitions a))))
        c (time (doall (take n (map #(sort (map count %)) b))))
        d (time (doall (take n (map #(apply max %) c))))]
    (println "done")
    (map (vec d) '(1 2 4 8 16 32 64))))

(profile 100)

;; "start"
;; "Elapsed time: 9.639319 msecs"
;; "Elapsed time: 3157.97671 msecs"
;; "Elapsed time: 5.33656 msecs"
;; "Elapsed time: 1.137731 msecs"
;; "done"

;; It appears that it is the function partitions which is slow

;; Here is the definition

(defn partitions [boxes]
  (set (map #(orbit boxes %) (range (count boxes)))))

;; For each element in the arrangement, it calls orbit, which itself runs through n iterations.
;; There's an order n*n thing going on here where we probably only need order n.

(def p (shuffle (range 10)))

;;Let's define a test permutation which we want to split into cycles

;; p [4 2 6 8 1 7 9 5 0 3]

(defn get-cycle [p a]
  (let [it (iterator p)]
    (loop [cycle (list a), g a]
      (let [next (it g)]
        (if (= next a) (reverse cycle)
            (recur (cons next cycle) next ))))))


(defn cycles [p]
  (loop [els (set p) cycles '()]
    (if (empty? els) cycles
        (let [cycle (get-cycle p (first els))]
          (recur (apply disj els cycle) (cons cycle cycles))))))

(defn partitions [p]
  (set (map #( apply sorted-set %) (cycles p))))


;; This is much faster.

;; If we go back and reprofile, then we now find that cumulative-frequency-sequence starts to dominate the
;; time for n=4000

;; This is its current definition:
(defn cumulative-frequency-sequence [largest-cycles-sequence]
  (map (fn[m] (frequencies (take m largest-cycles-sequence))) (iterate inc 1)))


;; It's recalculating the sum over the whole sequence every time


(def example (largest-cycles (boxes-seq 100)))
(take 5 example) ;; (86 91 52 30 99)


(defn running-reduce [f acc sq]
  (lazy-seq
   (if (empty? sq) '()
       (let [next-acc (f acc (first sq))]
         (cons next-acc (running-reduce f next-acc (rest sq)))))))

;;(running-reduce + 0 '(1 2 3 4 5))
(take 10 (running-reduce (fn [counts x] (assoc counts x (inc (get counts x 0)))) {} example))

(defn cumulative-frequency-sequence [largest-cycles-sequence]
  (drop 1 (running-reduce (fn [counts x] (assoc counts x (inc (get counts x 0)))) {} largest-cycles-sequence)))


(defn profile [n]
  (println "start")
  (let [a (time (doall (take n (boxes-seq 100))))
        b (time (doall (take n (largest-cycles a))))
        c (time (doall (take n (cumulative-frequency-sequence b))))
        d (time (doall (take n (wins-losses-seq 50 c))))
        e (time (doall (take n (map #(* 1.0 %) (wins-and-losses-ratio-seq d)))))]
    (println "done")
    (map (vec e) '(1 2 4 8 16 32 64))))

(profile 20000)

(take 5 (map #(* 1.0 %) (wins-and-losses-ratio-seq (wins-losses-seq 50 (cumulative-frequency-sequence (largest-cycles (boxes-seq 10)))))))





























;; Footnotes

;; This definition of orbit might be slightly more efficient. It is harder to understand, though:

;;(defn orbit [boxes n]
;;  (set
;;   (cons n
;;         (take-while #(not (= % n))
;;                     (drop 1 (iterations boxes n))))))

;; If we'd like to see the cycles explicitly

;;(defn cycles [boxes]
;; (map (fn [[s c]](take c (iterations boxes s)))
;;      (map (fn[set] [(first set) (count set)]) (partitions boxes))))




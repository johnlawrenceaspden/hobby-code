;; Experimentally testing lecture notes

(defn boxes[k] (vec (repeat k 0)))

(defn add-ball [boxes]
  (let [i (rand-int (count boxes))
        a (boxes i)]
    (assoc boxes i (inc a))))

(add-ball (boxes 8)) ;-> [0 0 1 0 0 0 0 0]
(nth (iterate add-ball (boxes 4)) 10) ;-> [3 3 3 1]

(defn arrangement "n boxes, k things" [n k]
  (nth (iterate add-ball (boxes k)) n))

(arrangement 10 8) ;-> [2 2 2 0 0 2 2 0]

(defn all-in-one [boxes]
  (let [[f & rst] (reverse (sort boxes))]
    (and (not (zero? f)) (every? zero? rst))))

(all-in-one [0 0]) ;-> false
(all-in-one [0 1]) ;-> true
(all-in-one [1 0]) ;-> true
(all-in-one [1 1]) ;-> false
(all-in-one [2 1]) ;-> false

(def trials (repeatedly (fn[] (arrangement 8 10))))

(take 3 trials) ;-> ([0 1 0 1 0 0 1 1 1 3] [0 4 1 0 2 0 0 1 0 0] [0 1 0 1 1 1 0 0 1 3])

(def oops (map all-in-one trials))

(take 3 oops) ;-> (false false false)

(frequencies (take 1000 oops)) ;-> {false 1000}
(frequencies (take 10000 oops)) ;-> {false 10000}
(frequencies (take 100000 oops)) ; -> {false 100000} ; 18 seconds
(frequencies (take 1000000 oops)) ; -> {false 1000000} ; 170 seconds

(def box2balls3 (repeatedly (fn[] (arrangement 3 2))))

(take 10 box2balls3)

(frequencies (take 10000 (map all-in-one box2balls3))) ;-> {false 7546, true 2454}

(frequencies (take 10000 box2balls3))

;;;;;;;;;;;;;;;;;;;;;; What about the XO swapping thing?

(def initial-state (vec (shuffle (concat (repeat 7 'X) (repeat 10 'O)))))

(defn swap [state]
  (let[i (rand-int (count state))
       j (rand-int (count state))
       a (state i)
       b (state j)]
    (-> state 
        (assoc j a)
        (assoc i b))))
    

initial-state

(def chain (iterate swap initial-state))

(nth chain 10) ;-> ;[O X X X O X X X O O O O O O X O O]

(count (partition-by identity '[O X X X O X X X O O O O O O X O O]))

(frequencies (take 100000 (for [ s (drop 100 chain)] (count (partition-by identity s)))))

;; three times out of 99000. (* 99900 0.000411) is 41. Sigh.
{2 3, 3 56, 4 500, 5 1898, 6 5675, 7 9975, 8 17614, 9 19345, 10 19406, 11 13512, 12 7726, 13 3182, 14 872, 15 236}

(defn swap2 [state]
  (let[i (rand-int (dec (count state)))
       j (inc i)
       a (state i)
       b (state j)]
    (-> state 
        (assoc j a)
        (assoc i b))))

(def chain2 (iterate swap2 initial-state))

(frequencies (take 100000 (for [ s (drop 100 chain2)] (count (partition-by identity s)))))

;; Doesn't look likely either!
{3 110, 4 402, 5 1820, 6 5393, 7 10672, 8 16472, 9 20905, 10 18497, 11 13933, 12 7481, 13 3197, 14 920, 15 198}

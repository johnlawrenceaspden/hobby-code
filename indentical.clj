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

(filter #(= 'O (first %))(partition-by identity '[O X X X O X X X O O O O O O X O O]))

(count (filter #(= 'O (first %))(partition-by identity '[O X X X O X X X O O O O O O X O O])))

(defn occupied [state]
  (count (filter #(= 'O (first %))(partition-by identity state))))

(frequencies (take 100000 (for [ s (drop 100 chain)] (occupied s))))
{4 30236, 5 36465, 3 10554, 2 1241, 6 17829, 7 3418, 8 236, 1 21}
;; three times out of 99000. (* 99900 0.000411) is 41. Sigh.

(defn swap2 [state]
  (let[i (rand-int (dec (count state)))
       j (inc i)
       a (state i)
       b (state j)]
    (-> state 
        (assoc j a)
        (assoc i b))))

(def chain2 (iterate swap2 initial-state))

(frequencies (take 100100 (for [ s (drop 100 chain2)] (occupied s))))
{6 18029, 5 36456, 4 30459, 7 3495, 3 10420, 2 943, 8 198}

(frequencies 
 (take 100100 
       (for [ s (drop 100 (iterate swap2 (shuffle initial-state)))] 
         (occupied s))))

{5 36816, 4 29803, 3 9572, 6 18823, 7 3754, 2 1028, 8 243, 1 61}
{5 37202, 4 29686, 3 9787, 2 1257, 6 18358, 7 3585, 8 181, 1 44}


;; Doesn't look likely either!
(/ 100000. 17 13 11) ;-> 41.13533525298231
;; Whatever process we're looking for should come up 41 times

(defn factorial[n] (reduce * (range 1 (inc n))))

(factorial 3)

(def pf (/ (* (factorial 10) (factorial 7)) (factorial 17))) ; 1/19448

(* 8. pf)
(* (/(* 9. 8 7) 2) pf)





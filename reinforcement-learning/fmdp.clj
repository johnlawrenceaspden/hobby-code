;; A finite markov decision process

(def p {[:A,1] [[1/6 3 :A]
                [2/6 2 :B]
                [3/6 1 :B]]
        [:A,2] [[1, 0, :B]]
        [:B,1] [[1,10, :A]]})

;; A policy

(def pi {:B [[1,1]]
         :A [[1,1]
             [1,2]]})

;; A random weighted choice function
(defn wrand 
  "given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

(wrand [1/6 2/6 3/6]) ; 0 ; 1 ; 2 ; 1 ; 0 ; 2 ; 2 ; 2 ; 2 ; 2 ; 1

;; adapted to choose from a vector of seqs whose first elements are their weights
(defn wchoose[v] (drop 1 (nth v (wrand (mapv first v)))))

(wchoose [[5 :A][2 :B][2 :C][1 :D]]) ; (:A) ; (:B) ; (:B) ; (:D) ; (:C) ; (:A) ; (:B) ; (:D) ; (:B)
(sort (frequencies (map first (take 1000 (repeatedly #(wchoose [[700 :A][200 :B][60 :C][39.99 :D]])))))) ; ([:A 705] [:B 184] [:C 65] [:D 46])



;; run the chain by hand
;; state A
(pi :A) ; [[1 1] [1 2]]
(wchoose (pi :A)) ; (2)
;; action 2
(wchoose (p [:A 2])) ; (0 :B)
;; reward 0
;; state B
(wchoose (pi :B)) ; (1)
;; action 1
(wchoose (p [:B 1])) ; (10 :A)
;; reward 10
;; state A
(wchoose (pi :A)) ; (2)
;; action 2
(wchoose (p [:A 2])) ; (0 :B)
;; reward 0
;; state B

;; automate that
(defn step[state]
  (let [ action (first (wchoose (pi state)))]
    (wchoose (p [state action]))))

(step :A) ; (0 :B)
(step :B) ; (10 :A)
(step :A) ; (1 :B)
(step :B) ; (10 :A)


;; run it to infinity
(iterate (fn [[reward state]] (step state)) [0 :A]) ; ([0 :A] (1 :B) (10 :A) (2 :B) (10 :A) (0 :B) (10 :A) (2 :B) (10 :A) (1 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (1 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (3 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (2 :B) (10 :A) (0 :B) (10 :A) (0 :B) (10 :A) (1 :B) (10 :A) (3 :A) (0 :B) (10 :A) (1 :B) (10 :A) (3 :A) (0 :B) (10 :A) (1 :B) (10 :A) (1 :B) (10 :A) (1 :B) (10 :A) (1 :B) (10 :A) (2 :B) (10 :A) (1 :B) (10 :A) (1 :B) (10 :A) (1 :B) (10 :A) (2 :B) (10 :A) (0 :B) (10 :A) (1 :B) (10 :A) (0 :B) (10 :A) (1 :B) (10 :A) (3 :A) (0 :B) (10 :A) (2 :B) ...)

;; take the first thousand rewards
(map first (take 1000 (iterate (fn [[reward state]] (step state)) [0 :A]))) ; (0 3 0 10 0 10 0 10 0 10 2 10 0 10 1 10 0 10 1 10 0 10 0 10 1 10 0 10 0 10 0 10 0 10 1 10 1 10 3 1 10 0 10 0 10 0 10 0 10 1 10 1 10 0 10 3 0 10 1 10 0 10 0 10 1 10 2 10 0 10 0 10 1 10 2 10 0 10 0 10 0 10 0 10 0 10 0 10 1 10 0 10 0 10 1 10 1 10 1 10 ...)


(defn average [sq] (/ (reduce + sq) (count sq)))

;; average them
(float (average (drop 1 (map first (take 1000000 (iterate (fn [[reward state]] (step state)) [0 :A])))))) ; 5.217604
(float (average (drop 1 (map first (take 1000000 (iterate (fn [[reward state]] (step state)) [0 :B])))))) ; 5.216444

;; 5.217 is the calculated expected value!


    

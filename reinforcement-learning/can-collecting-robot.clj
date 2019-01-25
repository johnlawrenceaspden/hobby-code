;; The Can-Collecting Robot

;;  Helpful Functions

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

;; the average of a finite sequence
(defn average [sq] (/ (reduce + sq) (count sq)))

;; round to two significant figures
(defn twosf   [x]  (float (/ (Math/round (* x 100.0)) 100))) 
(defn twosfs  [x]  (clojure.pprint/cl-format nil "~,2f" x))

(twosf  3.14159265358) ; 3.14  ;; close enough for government work
(twosfs 3.14159265358) ; "3.14" ;; type safety vs. annoying quote marks....


(map (juxt identity twosf twosfs) [1000000000000001/30787878787, 234N, 22/7, 0.555,-0.555 ,-1,-1.01, -1.001, 0, 3.14159265358,3.15,3.0,3.0001,3.01,3.005,3.0049,3.0051]) ; ([1000000000000001/30787878787 32480.31 "32480.31"] [234N 234.0 "234.00"] [22/7 3.14 "3.14"] [0.555 0.56 "0.56"] [-0.555 -0.56 "-0.56"] [-1 -1.0 "-1.00"] [-1.01 -1.01 "-1.01"] [-1.001 -1.0 "-1.00"] [0 0.0 "0.00"] [3.14159265358 3.14 "3.14"] [3.15 3.15 "3.15"] [3.0 3.0 "3.00"] [3.0001 3.0 "3.00"] [3.01 3.01 "3.01"] [3.005 3.01 "3.01"] [3.0049 3.0 "3.00"] [3.0051 3.01 "3.01"])


(def Rwait   1)   ;; cans collected while waiting
(def Rsearch 5)   ;; cans collected while searching
(def alpha   0.9) ;; battery decay constant high->low
(def beta    0.9) ;; battery decay constant low->fail


;; The state transition diagram expressed as the partial function p (state,action) -> (odds reward new-state)
;; (It's a partial function because not all actions are possible in all states)
;; (Note it's an odds version 1,2,3 -> 1/6 2/6 3/6 as probabilities)
;; p {[state, action] [[odds reward new-state]],.....}

(def p {[:H,:wait]       [[1           Rwait   :H]]
        [:H :search]     [[alpha       Rsearch :H]
                          [(- 1 alpha) Rsearch :L]]
        [:L,:wait]       [[1,          Rwait,  :L]]
        [:L,:search]     [[beta,       Rsearch,     :L]
                          [(- 1 beta), -3,     :L]]
        [:L,:recharge]   [[1,          0,      :H]]})


;; A policy
;; pi {state [[odds action]...], state [[....}
(def pi {:H [[1  :wait     ]
             [1  :search   ]]
         :L [[1  :recharge ]
             [1  :wait     ]
             [1  :search   ]]})

;; automate that
(defn step [state]
  (let [ action (first (wchoose (pi state)))]
    (cons action (wchoose (p [state action])))))

(step :H) ; (:wait 1 :H)
(step :H) ; (:search 5 :H)
(step :H) ; (:search 5 :H)
(step :H) ; (:wait 1 :H)
(step :H) ; (:search 5 :H)
(step :H) ; (:wait 1 :H)
(step :H) ; (:wait 1 :H)
(step :H) ; (:search 5 :H)
(step :H) ; (:search 5 :H)
(step :H) ; (:wait 1 :H)
(step :H) ; (:wait 1 :H)
(step :H) ; (:wait 1 :H)
(step :H) ; (:search 5 :L)
(step :L) ; (:search 10 :L)
(step :L) ; (:search 10 :L)
(step :L) ; (:search 10 :L)
(step :L) ; (:recharge 0 :H)
(step :H) ; (:wait 1 :H) 
(step :H) ; (:search 5 :H)




;; run it to infinity
(defn run [state] (iterate (fn [[action reward state]] (step state)) [:dummy :dummy state]))

(run :H) ; ([:dummy :dummy :H] (:search 5 :H) (:search 5 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:wait 1 :H) (:wait 1 :H) (:wait 1 :H) (:search 5 :L) (:search 10 :L) (:recharge 0 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:wait 1 :H) (:wait 1 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:search 5 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:wait 1 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:wait 1 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :L) (:search 10 :L) (:search 10 :L) (:recharge 0 :H) (:wait 1 :H) (:wait 1 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :L) (:recharge 0 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) (:wait 1 :H) (:search 5 :H) ...)

;; just rewards (get rid of reward 0, which doesn't exist in our notation/ is :dummy in our data structure)
(defn rewards [run]
  (map second (drop 1 run)))
                                        ;
(rewards (run :H)) ; (1 5 1 5 1 5 5 5 5 1 1 5 1 1 5 1 5 1 1 1 1 5 1 5 1 5 5 1 1 5 5 5 5 5 1 5 1 5 1 1 1 5 5 5 1 1 0 5 1 5 5 5 1 1 5 5 5 5 1 1 5 5 1 1 5 1 5 5 1 1 5 5 5 1 5 5 5 1 5 0 1 1 1 1 1 5 0 5 1 5 1 1 5 5 0 1 1 5 1 5 ...)






;; average them (careful not to look into the face of the gorgon...)
(twosf (average (rewards (take 10 (run :H))))) ; 2.78 ; 2.22 ; 2.22 ; 2.33
(twosf (average (rewards (take 10 (run :L))))) ; 1.78 ; 2.22 ; 2.22 ; 2.67


(twosf (average (rewards (take 100 (run :H))))) ; 2.8 ; 2.72 ; 2.77 ; 2.47
(twosf (average (rewards (take 100 (run :L))))) ; 2.87 ; 3.08 ; 2.95 ; 3.07

(twosf (average (rewards (take 1000 (run :H))))) ; 2.73 ; 2.77 ; 2.85 ; 2.76
(twosf (average (rewards (take 1000 (run :L))))) ; 2.8 ; 2.79 ; 2.76 ; 2.99

(twosf (average (rewards (take 10000 (run :H))))) ; 2.86 ; 2.83 ; 2.85 ; 2.85
(twosf (average (rewards (take 10000 (run :L))))) ; 2.84 ; 2.85 ; 2.8 ; 2.82

;; I think we can be reasonably confident that with our random policy, the long term average value of the automaton is ~2.8

;; Over ten iterations, it's not particularly clear that starting off with a full battery is better
(twosf (average (rewards (take 10 (run :H))))) ; 2.33 ; 2.78 ; 3.22 ; 2.78 ; 3.67
(twosf (average (rewards (take 10 (run :L))))) ; 2.22 ; 3.56 ; 1.78 ; 2.67 ; 0.89


;; But if we look at thousands of runs
(twosf (average (repeatedly 1000 #(average (rewards (take 10 (run :H))))))) ; 2.89 ; 2.95 ; 2.91 ; 2.92 ; 2.87 ; 2.87
(twosf (average (repeatedly 1000 #(average (rewards (take 10 (run :L))))))) ; 2.5  ; 2.49 ; 2.53 ; 2.52 ; 2.51 ; 2.51

;; Then it looks as though the high-battery state is worth ~2.9 and the low state ~2.5
    

;; The Can-Collecting Robot


;; Utility Functions

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



(defn average [sq] (/ (reduce + sq) (count sq)))

;; average them (careful not to look into the face of the gorgon...)
(float (average (rewards (take 10 (run :H))))) ; 4.111111  ; 3.2222223 ; 2.3333333 ; 2.3333333 ; 3.2222223 
(float (average (rewards (take 10 (run :L))))) ; 2.2222223 ; 3.1111112 ; 1.7777778 ; 3.5555556 ; 1.7777778 


(float (average (rewards (take 100 (run :H))))) ; 2.7777777 ; 2.989899  ; 2.4242425 ; 2.9191918
(float (average (rewards (take 100 (run :L))))) ; 2.838384  ; 2.9494948 ; 2.8282828 ; 2.7474747

(float (average (rewards (take 1000 (run :H))))) ; 2.8388388 ; 2.7497497 ; 2.903904 ; 2.873874 ; 2.821822 ; 2.8088088 
(float (average (rewards (take 1000 (run :L))))) ; 2.8208208 ; 2.9469469 ; 2.867868 ; 2.7467468 ; 2.9229228 ; 2.7667668 

(float (average (rewards (take 10000 (run :H))))) ; 2.8353837 ; 2.8274827 ; 2.849385 ; 2.839984  ; 2.7978797 
(float (average (rewards (take 10000 (run :L))))) ; 2.8379838 ; 2.8384838 ; 2.8164816 ; 2.848985 ; 2.8355834 

;; I think we can be reasonably confident that with our random policy, the long term average value of the automaton is 2.8


    

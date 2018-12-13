;; K armed bandits, chapter 2 of Sutton's book

;; q*(a) is the true expectation of the action a
;; Q_t(a) is the current estimate (at time t)


;; A 2 armed bandit
(defn bandit [action]
  (if (= action :arms?) [1 2]
      (case action
        1 (if (< (rand) 0.5) 4 0)
        2 (if (< (rand) 0.2) 5 0)
        :oops)))

(map bandit [1 2 1 2 1 2 1 2 :arms? :oops]) ; (0 5 0 0 4 5 4 0 :oops)

;; initial state, no data
(defn initial-state [bandit]
  (into {} (for [k (bandit :arms?)] [k '()])))

(initial-state bandit) ; {1 (), 2 ()}

(defn mapvals [m f] (into {} (for [[k v] m] [k (f v)])))
(defn average-list [l] (if (empty? l) 0 (/ (reduce + l) (count l))))

(defn Q [state] (mapvals state average-list))

(Q (initial-state bandit)) ; {1 0, 2 0}

(defn max-keys [m]
  (let [slist (reverse (sort-by second m)) 
        [_ max] (first slist)]
    (take-while #(= (second %) max) slist)))

(max-keys {1 0, 2 0, 3 -1 , 4 -3, 5 2, 6 2}) ; ([6 2] [5 2])

(defn greedy-action [qmap]
  (first (rand-nth (max-keys qmap))))

(greedy-action (Q (initial-state bandit))) ; 2

(initial-state bandit) ; {1 (), 2 ()}
;; The Qs are (by definition):
(Q (initial-state bandit)) ; {1 0, 2 0}

;; choose at random
(greedy-action (Q (initial-state bandit))) ; 2

;; bandit's response
(bandit 2) ; 5

;; record it
(update-in {1 '() 2 '()} [2] #(cons 5 %)) ; {1 (), 2 (5)}

;; new state
{1 (), 2 (5)}

(greedy-action (Q {1 (), 2 '(5)})) ; 2

(bandit 2) ; 0

(update-in {1 '(), 2 '(5)} [2] #(cons 0 %)) ; {1 (), 2 (0 5)}




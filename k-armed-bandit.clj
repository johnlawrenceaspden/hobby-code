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

(Q (initial-state bandit)) ; 


(defn greedy-action [qmap])


(second (first (sort-by second {1 0, 2 0}))) ; ([1 0] [2 0])
(take-while #(= (second %) 0) {1 0, 2 0, 3 -1})
  
  
(greedy-action (Q (initial-state bandit)))

(initial-state bandit) ; {1 (), 2 ()}
;; The Qs are (by definition):
(Q (initial-state bandit)) ; {1 0, 2 0}
;; choose at random
(rand-nth [1 2]) ; 2
;; bandit's response
(bandit 2) ; 5
(update-in {1 '() 2 '()} [1] #(cons 5 %))

;; new state
{1 (5), 2 ()}
;; Q
{1 5, 2 0}
;; choose 1
(bandit 1) ; 4
(update-in {1 '(5) 2 '()} [1] #(cons 4 %)) ; {1 (4 5), 2 ()}




(average-list '()) ; 0
(average-list '(1)) ; 1
(average-list '(1 2)) ; 3/2

;; new state
{1 (4 5), 2 ()}

(mapvals {1 '(4 5), 2 '()} average-list) ; {1 9/2, 2 0}

;; choose 1
(bandit 1) ; 0
(update-in  {1 '(4 5), 2 '()} [1] #(cons 0 %)) ; {1 (0 4 5), 2 ()}
(mapvals {1 '(0 4 5), 2 '()} average-list) ; {1 3, 2 0}


















(def available-actions [1 2])

(def actions [1 2 1 2 2 2])
(def rewards [5 0 0 3 0 0])



;; two different ways to define Q as in (2.1) in the book
(defn Q [actions rewards]
  (fn [action]
    (let [l (map second (filter (fn[[a r]] (= a action)) (map vector actions rewards)))]
      (/ (reduce + l)(count l)))))

;; more like the book definition
(defn indicator [l v]
  (map #(if (= % v) 1 0) l))

(defn Q [actions rewards]
  (fn [action]
    (let [I (indicator actions action)]
      (/ (reduce + (map * I rewards))
         (reduce + I)))))


((Q actions rewards) 1) ; 5/2
((Q actions rewards) 2) ; 3/4

;; greedy action is the one that's best so far

(defn argvals [F vals]
  (sort-by first (map (juxt F identity) vals)))

(argvals (Q actions rewards) available-actions) ; ([3/4 2] [5/2 1])

(defn argmax [F vals]
  (second (last (argvals F vals))))

(argmax (Q actions rewards) available-actions) ; 1






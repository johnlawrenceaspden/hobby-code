;; K armed bandits, chapter 2 of Sutton's book

;; q*(a) is the true expectation of the action a
;; Q_t(a) is the current estimate (at time t)

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

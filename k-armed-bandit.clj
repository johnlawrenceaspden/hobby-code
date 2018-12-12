;; K armed bandits, chapter 2 of Sutton's book

;; q*(a) is the true expectation of the action a
;; Q_t(a) is the current estimate (at time t)

(def actions [1 2 1 2 2 2])
(def rewards [5 0 0 3 0 0])

;; two different ways to define Q as in (2.1) in the book
(defn Q [actions rewards action]
  (let [l (map second (filter (fn[[a r]] (= a action)) (map vector actions rewards)))]
    (/ (reduce + l)(count l))))

;; more like the book definition
(defn indicator [l v]
  (map #(if (= % v) 1 0) l))

(defn Q [actions rewards action]
  (let [I (indicator actions action)]
    (/ (reduce + (map * I rewards))
       (reduce + I))))


(Q actions rewards 1) ; 5/2
(Q actions rewards 2) ; 3/4



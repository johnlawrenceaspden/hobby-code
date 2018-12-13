;; K armed bandits, chapter 2 of Sutton's book


;; First, I want to define a few general utility functions that will come in handy later

;; mapvals applies a function to every value in a map, returning a new map with the same keys
(defn mapvals [m f] (into {} (for [[k v] m] [k (f v)])))

(mapvals {} inc) ; {}
(mapvals {:a 1} inc) ; {:a 2}
(mapvals {:a 1, :b 2} inc) ; {:a 2, :b 3}
(mapvals {:a 1, :b 2, :c 3} #(* % %)) ; {:a 1, :b 4, :c 9}


;; average-list tells us the average value of a list of numbers, with a default if the list is empty.
(defn average-list [lst default] (if (empty? lst) default (/ (reduce + lst) (count lst))))

(average-list (list 1 2 3 4 5) 0) ; 3
(average-list (list) 10) ; 10
(average-list (list 1) 2) ; 1


;; max-keys finds the keys with the highest value in a map, and returns a map with just these keys
(defn max-keys [m]
  (let [slist (reverse (sort-by second m)) 
        [_ max] (first slist)]
    (take-while #(= (second %) max) slist)))

(max-keys {}) ; ()
(max-keys {1 0}) ; ([1 0])
(max-keys {1 0, 2 0}) ; ([2 0] [1 0])
(max-keys {1 0, 2 1}) ; ([2 1])
(max-keys {1 0, 2 1, 3 -1 , 4 -3, 5 2, 6 2}) ; ([6 2] [5 2])


;; A 2 armed bandit
(defn bandit [action]
  (if (= action :arms?) [:right :left]
      (case action
        :right (if (< (rand) 0.5) 4 0)
        :left (if (< (rand) 0.2) 5 0)
        :oops!!)))

(bandit :arms?) ; [:right :left]
(map bandit [:right :left]) ; (4 0)


;; We'd like to record what goes on in order to learn from it

;; initial state, no data
(defn initial-state [bandit]
  (into {} (for [k (bandit :arms?)] [k (list)])))

(initial-state bandit) ; {:right (), :left ()}

;; When we get a new action reward pair, we'll update our state
(defn update-state [state [action reward]]
  (update-in state [action] #(conj % reward)))



(update-state {:right (), :left ()} [:right 2]) ; {:right (2), :left ()}
(reduce update-state {:right (), :left ()}
            [[:right 2] [:left 3]  [:right 4] [:right 5]]) ; {:right (5 4 2), :left (3)}

(update-state (initial-state bandit) [(rand-nth (bandit :arms?)) 2]) ; {:right (), :left (2)}


;; q*(a) is the true expectation of the action a
;; Q_t(a) is the current estimate (at time t)

;; We'll use as our estimate of the value of an action the average value seen so far, or zero if we have no information
(defn Q [state] (mapvals state #(average-list % 0)))

(Q '{:right (5 4 2), :left (3)}) ; {:right 11/3, :left 3}
(Q '{:right (5 4 2), :left ()}) ; {:right 11/3, :left 0}
(Q (initial-state bandit)) ; {:right 0, :left 0} 
(Q (update-state (initial-state bandit) [(rand-nth (bandit :arms?)) 2])) ; {:right 0, :left 2}


;; The greedy action is the one with the highest expected value
;; if there is a tie, we choose at random
(defn greedy-action [estimates]
  (first (rand-nth (max-keys estimates))))

(greedy-action '{:right 10, :left 3}) ; :right
(greedy-action '{:right 10, :left 3 :centre 20}) ; :centre
(greedy-action '{:right 10, :left 3 :centre 3}) ; :right
(greedy-action '{:right 3, :left 3 :centre 3}) ; :right ; :right ; :centre ; :right ; 

(greedy-action (Q '{:right (5 4 2), :left (3)})) ; :right
(greedy-action (Q '{:right (), :left (3)})) ; :left
(greedy-action (Q (initial-state bandit))) ; :left



;; Our first try at a learning algorithm will be 'by hand', as it were.

;; We'll always make the 'greedy' choice.

;; At first, we have no records to go on
(initial-state bandit) ; {:right (), :left ()}

;; expected values for both levers are therefore zero
(Q (initial-state bandit)) ; {:right 0, :left 0}

;; so the greedy action will get chosen at random
(greedy-action (Q (initial-state bandit))) ; :left

;; in this case, we've chosen :left, and the bandit's response is
(bandit :left) ; 0 

;; record it
(update-state (initial-state bandit) [:left 0]) ;

;; and we have a new state
'{:right (), :left (0)}

;; new estimates
(Q '{:right (), :left (0)}) ; {:right 0, :left 0}

;; again, choose at random
(greedy-action (Q '{:right (), :left (0)})) ; :left

;; it's not feeling very generous
(bandit :left) ; 0

(update-state '{:right (), :left (0)} [:left 0]) ; {:right (), :left (0 0)}

;; new state:
'{:right (), :left (0 0)}

(Q '{:right (), :left (0 0)}) ; {:right 0, :left 0}

;; this time we choose :right
(greedy-action (Q '{:right (), :left (0 0)})) ; :right

;; and the bandit pays out! 
(bandit :right) ; 4

(update-state '{:right (), :left (0 0)} [:right 4]) ; {:right (4), :left (0 0)}

;; You get the idea......

;; Let's automate that....



















(defn step [state bandit]
  (let [a (greedy-action (Q state))
        r (bandit a)
        new-state (update-in state [a] #(cons r %))]
    [a r new-state]))

(step (initial-state bandit) bandit) ; [2 0 {1 (), 2 (0)}]

(defn do-step [[a r state]]
  (step state bandit))

(do-step (step (initial-state bandit) bandit)) ; [1 4 {1 (4 4), 2 ()}]
(do-step (do-step (step (initial-state bandit) bandit))) ; [1 4 {1 (4 4 0), 2 ()}]

(take 10 (iterate do-step (step (initial-state bandit) bandit)))
(
 [2 0 {1 (), 2 (0)}]
 [2 0 {1 (), 2 (0 0)}]
 [1 4 {1 (4), 2 (0 0)}]
 [1 0 {1 (0 4), 2 (0 0)}]
 [1 4 {1 (4 0 4), 2 (0 0)}]
 [1 0 {1 (0 4 0 4), 2 (0 0)}]
 [1 0 {1 (0 0 4 0 4), 2 (0 0)}]
 [1 0 {1 (0 0 0 4 0 4), 2 (0 0)}]
 [1 0 {1 (0 0 0 0 4 0 4), 2 (0 0)}]
 [1 0 {1 (0 0 0 0 0 4 0 4), 2 (0 0)}]) ;

(defn run [bandit initializer step]
  (let [initial-state (initializer bandit)
        do-step (fn [[a r state]] (step state bandit))]
    (iterate do-step initial-state)))

(run bandit initial-state step)

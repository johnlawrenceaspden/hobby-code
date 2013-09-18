;; Scheduling

;; Consider a list of jobs with lengths and weights

(def jobs [{:length 2 :weight 2} {:length 1 :weight 3} {:length 3 :weight 1}])

;; The cost of running each job is the completion time multiplied by the weight

(defn cost [jobs]
  (let [lengths (map :length jobs)
        weights (map :weight jobs)
        completions (reductions + lengths)
        costs (map * weights completions)
        cost (reduce + costs)]
    [ cost costs weights completions lengths]))

(cost jobs) ;-> [19 (4 9 6) (2 3 1) (2 3 6) (2 1 3)]

;; We might actually take this model literally. Consider a company
;; which is paying daily penalties on several jobs which have overrun,
;; and trying to work out where to put the energy of its staff.

;; Different ways of ordering the jobs result in different costs:
(map cost (clojure.math.combinatorics/permutations jobs)) 
;; ([15 (3 6 6) (3 2 1) (1 3 6) (1 2 3)]
;;  [19 (3 4 12) (3 1 2) (1 4 6) (1 3 2)]
;;  [19 (4 9 6) (2 3 1) (2 3 6) (2 1 3)]
;;  [27 (4 5 18) (2 1 3) (2 5 6) (2 3 1)]
;;  [27 (3 12 12) (1 3 2) (3 4 6) (3 1 2)]
;;  [31 (3 10 18) (1 2 3) (3 5 6) (3 2 1)])

;; It looks as though our best (cheapest) orderings are those where short, high-priority jobs
;; come first.

;; Let's make a list of jobs at random
(def random-jobs (take 7 (repeatedly (fn[] {:weight (rand-int 10) :length (rand-int 10)}))))

;; And find by brute force the optimal arrangement
(reduce min 
        (map (comp first cost) 
             (clojure.math.combinatorics/permutations random-jobs))) ;-> 412

;; The cost of this operation is gigantic, factorial in the number of jobs. How can we do better?

;; Consider some possible functions that we might use to order our jobs, which
;; rank jobs higher as their priority is higher, and lower as their length is higher

(def difference-cost    (fn[{:keys [weight length]}] (- weight length)))
(def ratio-cost         (fn[{:keys [weight length]}] (/ weight length)))
(def sq-ratio-cost      (fn[{:keys [weight length]}] (let [s (/ weight length)] (* s s))))
(def length-square-ratio-cost         (fn[{:keys [weight length]}] (/ weight (* length length))))

;; Sorting the jobs by ratio-cost finds the optimal arrangement
(cost (reverse (sort-by ratio-cost random-jobs))) ; [412 (18 28 36 90 96 115 29) (9 7 6 9 6 5 1) (2 4 6 10 16 23 29) (2 2 2 4 6 7 6)]

;; But sorting by difference-cost finds a decent, but not optimal arrangement
(cost (reverse (sort-by difference-cost random-jobs))) ; [428 (18 54 56 60 96 115 29) (9 9 7 6 6 5 1) (2 6 8 10 16 23 29) (2 4 2 2 6 7 6)]

;; Sorting by square-ratio also works
(cost (reverse (sort-by sq-ratio-cost random-jobs))) ; [412 (18 28 36 90 96 115 29) (9 7 6 9 6 5 1) (2 4 6 10 16 23 29) (2 2 2 4 6 7 6)]

;; As does the length-square version
(cost (reverse (sort-by length-square-ratio-cost random-jobs))) ;-> [412 (18 28 36 90 96 115 29) (9 7 6 9 6 5 1) (2 4 6 10 16 23 29) (2 2 2 4 6 7 6)]


;; I think it's interesting to try to work out which of these
;; criteria, if any, will reliably order the jobs in the best way.

;; The answer is not terribly obvious, but very simple once you see it!

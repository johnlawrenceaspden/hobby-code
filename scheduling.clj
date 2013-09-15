;; Algorithms II : Scheduling

;; Consider a list of jobs with lengths and weights

(def jobs [{:length 1 :weight 3} {:length 2 :weight 2} {:length 3 :weight 1}])

;; The cost of running each job is the completion time multiplied by the weight

(defn cost [jobs]
  (let [lengths (map :length jobs)
        weights (map :weight jobs)
        completions (reductions + lengths)
        costs (map * weights completions)
        cost (reduce + costs)]
    [ cost costs weights completions lengths]))

(cost jobs) ;-> [15 (3 6 6) (3 2 1) (1 3 6) (1 2 3)]

(map cost (clojure.math.combinatorics/permutations jobs)) 
;; ([15 (3 6 6) (3 2 1) (1 3 6) (1 2 3)]
;;  [19 (3 4 12) (3 1 2) (1 4 6) (1 3 2)]
;;  [19 (4 9 6) (2 3 1) (2 3 6) (2 1 3)]
;;  [27 (4 5 18) (2 1 3) (2 5 6) (2 3 1)]
;;  [27 (3 12 12) (1 3 2) (3 4 6) (3 1 2)]
;;  [31 (3 10 18) (1 2 3) (3 5 6) (3 2 1)])

;; It looks as though our best (cheapest) orderings are those where short, high-priority jobs
;; come first.
(def random-jobs (take 5 (repeatedly (fn[] {:weight (rand-int 10) :length (rand-int 10)}))))



(reduce min (map (comp first cost) (clojure.math.combinatorics/permutations random-jobs))) ;-> 149

(def sorted-jobs (reverse (sort-by (fn[{:keys [weight length]}] (/ weight length)) random-jobs)))

(cost sorted-jobs) ;-> [149 (4 35 45 65) (4 7 5 5) (1 4 4 4) (1 5 9 13)]





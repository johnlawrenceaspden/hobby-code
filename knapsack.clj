;; Knapsack Problem : Dynamic Programming

;; Suppose you've got twelve pounds
(def budget 12)

;; And there's a thing that costs a pound, but is worth 20.
;; And another thing that costs 3, but is worth 30
;; And another thing that costs 3, but is worth 21
;; And a thing that costs 6 but is worth 40
(def things (map (fn[[c v]] {:cost c :value v}) [[1 20][3 30][3 21][6 40]]))

(defn price [things]    (reduce + (map :cost  things)))
(defn evaluate [things] (reduce + (map :value things)))

(evaluate things) ;-> 111
(price things) ;-> 13

;; So there's 111's worth of things going for 13, but you can't buy everything.

;; What do you buy? 

;; Well, if you're a cynic
(evaluate
 (let [order   (sort-by :cost things)
       baskets (reductions conj '() order)]
   (last (take-while #(<= (price %) budget) baskets)))) ;-> 71

;; Then you come away with 71's worth

;; And if you're an idealist
(evaluate
 (let [order   (reverse (sort-by :value things))
       baskets (reductions conj '() order)]
   (last (take-while #(<= (price %) budget) baskets)))) ;-> 91

;; Then you do slightly better
(evaluate
 (let [order   (reverse (sort-by (fn [{:keys[value cost]}](/ value cost)) things))
       baskets (reductions conj '() order)]
   (last (take-while #(<= (price %) budget) baskets))))

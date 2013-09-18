;; The Knapsack Problem

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

(defn value [sorted-things]
  (evaluate
   (let [order   sorted-things
         baskets (reductions conj '() order)]
     (last (take-while #(<= (price %) budget) baskets)))))

;; Well, if you're a cynic
(value (sort-by :cost things)) ;-> 71
;; Then you come away with 71's worth

;; And if you're an idealist
(value (reverse (sort-by :value things))) ;-> 91
;; Then you do way better with 91

;; A more cunning approach is to take things in order of their price/value ratio
(value (reverse (sort-by (fn [{:keys[value cost]}] (/ value cost)) things))) ;-> 71
;; Sadly that does worse than the approach that only pays attention to the value.

;; So it seems that out of the three natural-seeming 'greedy algorithms', the best solution is 91


;; Yet another approach is to exhaustively search the space of possibilities:

(defn subsets [things]
  (if (empty? things) '(())
      (let [srt (subsets (rest things))]
        (concat (map #(cons (first things) %) srt) srt))))

 
(reverse (sort-by second (for [i (subsets things)] [(price i) (evaluate i)]))) 
;-> ([13 111] [12 91] [10 90] [10 81] [7 71] [9 70] [9 61] [7 60] [6 51] [4 50] [4 41] [6 40] [3 30] [3 21] [1 20] [0 0])

;; Which tells us that the best combination is unaffordable, so we
;; have to settle for the second best, which is paying 12 to get 91,
;; which the idealist has been trying to tell us all along.

;; But the idealistic approach is unlikely to work in the general case. 

;; Consider a thing which is worth a lot, but horribly expensive, and
;; lots of other things which are worth a fair bit and dirt cheap.

;; Personally my money would have been on the 'buy things in order of
;; price/value ratio' approach, but we above that that fails in at
;; least one easy case.

;; So it appears that if we are faced with a problem like this, ( and
;; there are many such problems ), then we are doomed.

;; Exhaustive search is not feasible once you've got more than a very
;; few items, and yet the various greedy algorithms above get the
;; wrong answers.

;; And yet if you write down a knapsack problem like this, you will
;; not find it appallingly difficult to pick the best arrangement.

;; There is a certain tradition at this point of exclaiming 'The HUMAN
;; BRAIN is performing a COMPUTATION INFEASIBLE for a CLASSICAL
;; COMPUTER', and then going on to derive your favourite philosophical
;; position on the nature of consciousness, which will miraculously
;; turn out to be whatever it was you thought before you contemplated
;; the problem in question.

;; But wait ...



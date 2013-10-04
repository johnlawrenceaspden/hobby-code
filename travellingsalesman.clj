;; On the Travelling of Salesmen

;; Four vertices, 1, 2, 3, 4

(def vertexset #{1,2,3,4})

;; Leads to 6 edges in the complete graph

(def edgeset {[1 2] 2, [1 3] 4, [1 4] 5,  [2 3] 2, [2 4] 3, [3 4] 4})

;; Which we can represent as a distance function

(defn make-d [edgeset] 
  (fn [a b] (or (edges [a b]) (edges [b a]))))

;; Have we got them all?
(for [a vertexset b (disj vertexset a)] [a b ((make-d edgeset) a b)])
;-> ([1 2 2] [1 3 4] [1 4 5] [2 1 2] [2 3 2] [2 4 3] [3 1 4] [3 2 2] [3 4 4] [4 1 5] [4 2 3] [4 3 4])

;; A tour in such a graph is a permutation of the list of vertices

(defn scissor [a lst]
  (for [i (range (inc (count lst)))] (concat (take i lst) (list a) (drop i lst))))

(defn perms [lst]
  (if (empty? lst) '(())
      (mapcat (partial scissor (first lst)) (perms (rest lst)))))

(perms vertexset) ;-> ((1 2 3 4) (2 1 3 4) (2 3 1 4) (2 3 4 1) (1 3 2 4) (3 1 2 4) (3 2 1 4) (3 2 4 1) (1 3 4 2) (3 1 4 2) (3 4 1 2) (3 4 2 1) (1 2 4 3) (2 1 4 3) (2 4 1 3) (2 4 3 1) (1 4 2 3) (4 1 2 3) (4 2 1 3) (4 2 3 1) (1 4 3 2) (4 1 3 2) (4 3 1 2) (4 3 2 1))

;; Which represents a traversal of selected edges in a cycle

(defn edgelist [tour]
  (for [[a b] (cons (list (last tour) (first tour)) (partition 2 1 tour))] [a b]))

(edgelist '(1 2 3 4)) ;-> ([4 1] [1 2] [2 3] [3 4])

;; And such tours have an associated cost:
(defn make-cost [edgeset]
  (let [d (make-d edgeset)]
    (fn [tour]
      (reduce + (for [[a b] (edgelist tour)] (d a b))))))

((make-cost edgeset) '(1 2 3 4) ) ;-> 13

(distinct (map (make-cost edgeset) (perms vertexset))) ;-> (13 14)

;; In this case it appears that 1 2 3 4 is a minimal cost tour. 

;; Finding such a tour is known as the Travelling Salesman Problem.

;; Behold, I have solved the problem in the time it took to state it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There is a problem with this approach

(take 10 (map (comp count perms) (reductions conj #{} (range))))
;; (1 1 2 6 24 120 720 5040 40320 362880)

;; which reminds one of

(reductions * (range 1 20)) ;-> (1 2 6 24 120 720 5040 40320 362880 3628800 39916800 479001600 6227020800 87178291200 1307674368000 20922789888000 355687428096000 6402373705728000 121645100408832000)

;; If we've got a trillion operations to burn, then we might be able to solve this problem for a graph with 14 vertices.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There is a way to do marginally less miserably

;; The minimal tour must go through 1. 

;; If we knew the cheapest path from 1 to 2 that went through 3 and 4 (once and once only)
;; and the cheapest path from 1 to 3 that went through 2 and 4 (oaoo)
;; and the cheapest path from 1 to 4 that went through 2 and 3 (o)

;; Then we'd be done, because we'd add the closing edge to each of these paths to make 3 separate tours
;; and then we'd pick the cheapest one.

;; So we need to summon the recursion fairies

;; And the fairies that we need to summon need to be able to answer questions of the following general form

;; 'What, oh fairy, is the cheapest path from 1 to j passing exactly
;; once through every vertex of the set S, which is a subset of the
;; vertices of the graph which does not contain either 1 or j?'

;; And the spell which summons such fairies is this spell
(defn cpath [j vset d]
  (cond (empty? vset) (d 1 j)
        (= (count vset) 1) (+ (d 1 (first vset)) (d (first vset) j))
        :else (apply min
                (for [ v vset] 
                  (+ (d v j) (cpath v (disj vset v) d))))))


;; Given such an incantation, we can work out the lengths of various short excursions taking in various points of interest
(cpath 1 #{}  (make-d edgeset)) ;-> nil
(cpath 2 #{}  (make-d edgeset)) ;-> 2
(cpath 3 #{2} (make-d edgeset)) ;-> 4

;; Including some which are nearly tours
(cpath 4 #{2 3} (make-d edgeset)) ;-> 8
(cpath 3 #{2 4} (make-d edgeset)) ;-> 9
(cpath 2 #{3 4} (make-d edgeset)) ;-> 11

;; Amazingly, when we create the final tours, they all end up having the same value
(+ (cpath 4 #{2 3} (make-d edgeset)) ((make-d edgeset) 4 1)) ;-> 13
(+ (cpath 3 #{2 4} (make-d edgeset)) ((make-d edgeset) 3 1)) ;-> 13
(+ (cpath 2 #{3 4} (make-d edgeset)) ((make-d edgeset) 2 1)) ;-> 13

;; So perhaps we only need to ask for one of them

(defn tsp [vertexset d]
    (+ (cpath 2 (disj (disj vertexset 2) 1) d) (d 2 1)))

(tsp vertexset (make-d edgeset)) ;-> 13

;; Now, if you look carefully, you will find that, given a graph with n vertices, the first call
;; here makes n-2 subcalls, which each make n-3 subcalls, which each make n-4 subcalls, and so on

;; So that the running time of this problem also goes like the factorial

(time (tsp #{1 2 3 4 5} (fn [a b] 1))) ;-> 5
"Elapsed time: 1.1352 msecs"
(time (tsp #{1 2 3 4 5 6} (fn [a b] 1))) ;-> 6
"Elapsed time: 1.457029 msecs"
(time (tsp #{1 2 3 4 5 6 7} (fn [a b] 1))) ;-> 7
"Elapsed time: 3.021475 msecs"
(time (tsp #{1 2 3 4 5 6 7 8} (fn [a b] 1))) ;-> 8
"Elapsed time: 12.281457 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9} (fn [a b] 1))) ;-> 9
"Elapsed time: 55.585345 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9 10} (fn [a b] 1))) ;-> 10
"Elapsed time: 416.957839 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9 10 11} (fn [a b] 1))) ;-> 11
"Elapsed time: 3365.925792 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9 10 11 12} (fn [a b] 1))) ;-> 12
"Elapsed time: 33854.042469 msecs"
  

(defn p2 [sq] (with-out-str (doseq [i sq] (printf "%2.2f " i))))

(defn successive-ratios [sq] (map (fn[[a b]] (float (/ b a))) (partition 2 1 sq)))

(p2 (successive-ratios '(1.13 1.45 3.02 12.2 55.5 416. 3365.1 33854.0)))
;-> "1.28 2.08 4.04 4.55 7.50 8.09 10.06 "


;; and yes, since you ask, I *did* fiddle these timings.

;; Now normally at this point, when we think of a clever recurrence relation and implement it in a straightforward manner,
;; memoization will turn it from a superexponential horror into a clever dynamic programming algorithm and it will run in O(n) or O(n^2) or something neat like that

(def cpath (memoize cpath))
(time (tsp #{1 2 3 4 5} (fn [a b] 1)))
"Elapsed time: 1.424132 msecs"
(time (tsp #{1 2 3 4 5 6} (fn [a b] 1)))
"Elapsed time: 2.55207 msecs"
(time (tsp #{1 2 3 4 5 6 7} (fn [a b] 1)))
"Elapsed time: 5.633396 msecs"
(time (tsp #{1 2 3 4 5 6 7 8} (fn [a b] 1)))
"Elapsed time: 15.057154 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9} (fn [a b] 1)))
"Elapsed time: 31.793358 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9 10} (fn [a b] 1)))
"Elapsed time: 73.710072 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9 10 11} (fn [a b] 1)))
"Elapsed time: 217.877989 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9 10 11 12} (fn [a b] 1))) ;-> 12
"Elapsed time: 770.114412 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9 10 11 12 13} (fn [a b] 1))) ;-> 13
"Elapsed time: 2395.936944 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9 10 11 12 13 14} (fn [a b] 1))) ;-> 14
"Elapsed time: 8114.328645 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15} (fn [a b] 1))) ;-> 15
"Elapsed time: 29191.135966 msecs"
(time (tsp #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16} (fn [a b] 1))) ;-> 16
"Elapsed time: 125260.112064 msecs"

;; We've got a bit quicker. If we've got 30 seconds to throw at the problem then we can solve it for n=15 rather than for n=12

(p2 (successive-ratios '(1.42 2.55 5.63 15. 31. 73. 217. 770. 2395. 8114. 29191. 125260.)))
;-> "1.80 2.21 2.66 2.07 2.35 2.97 3.55 3.11 3.39 3.60 4.29 "

;; It still looks superexponential, but the growth rate isn't quite as insane as it was. 

;; In detail
(p2 (map / '(1.42 2.55 5.63 15. 31. 73. 217. 770. 2395. 8114. 29191. 125260.) '(1.13 1.45 3.02 12.2 55.5 416. 3365.1 33854.0))) 
;-> "1.26 1.76 1.86 1.23 0.56 0.18 0.06 0.02 "

;; It seems that an early slowdown is being repaid by big speedups in larger problems

;; In fact the growth rate for the memoized version looks greater than it should be (around 4 rather than around 2.22)

;; A quick estimate of the number of subproblems to be solves gives O(n^2 2^n), which is superexponential
;; but which settles down to roughly doubling with every extra vertex

(def dynamic (map (fn[n] (* (* n n)(reduce * (repeat n 2)))) (iterate inc 1)))
;-> (0 2 16 72 256 800 2304 6272 16384 41472 102400 247808 589824 1384448 3211264 7372800 16777216 37879808 84934656 189267968 419430400 924844032 2030043136 4437573632 9663676416 20971520000 45365592064 ...)
(p2 (successive-ratios (take 20 dynamic))) ;-> "8.00 4.50 3.56 3.13 2.88 2.72 2.61 2.53 2.47 2.42 2.38 2.35 2.32 2.30 2.28 2.26 2.24 2.23 2.22 "

;; Whereas the brute force search and unmemoized versions should go more like O(n!) 

(def brute (reductions * (iterate inc 1)))
;-> (1 2 6 24 120 720 5040 40320 362880 3628800 39916800 479001600 6227020800 87178291200 1307674368000 20922789888000 355687428096000 6402373705728000 121645100408832000)

;; Which obviously:
(p2 (successive-ratios (take 20 brute))) ;-> "2.00 3.00 4.00 5.00 6.00 7.00 8.00 9.00 10.00 11.00 12.00 13.00 14.00 15.00 16.00 17.00 18.00 19.00 20.00 "

;; I wonder why my memoized version isn't doing better. 






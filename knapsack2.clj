;; The Knapsack Problem : Solution by Dynamic Programming

;; Suppose you've got a budget of twelve.
(def budget 12)

;; And some things
(def things (map (fn[[c v]] {:cost c :value v}) [[1 20][3 30][3 21][6 40]]))

things ;-> ({:cost 1, :value 20} {:cost 3, :value 30} {:cost 3, :value 21} {:cost 6, :value 40})

(defn price [things]    (reduce + (map :cost  things)))
(defn evaluate [things] (reduce + (map :value things)))

(evaluate things) ;-> 111
(price things) ;-> 13

;; And you want to know, given your budget, how much of the value you can get.

;; The trick is to notice that there's a structure to this problem.

;; Imagine that a semi-good fairy is happy to tell you the answer to all simpler knapsack questions

;; She will, for instance, tell you what you should buy if you only have a budget of 11 

;; And she will tell you what you should buy if you have a budget of twelve, but one of the items is not available.

;; But she will not tell you the answer to the question you want answered.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You reason as follows:

;; Should I buy the first thing, with the value of 20 and the cost of 1? 

;; It seems obvious that you should. It's very small and it's worth a lot.

;; But if you do, that leaves you with a budget of 11.

;; So you say to the fairy

;; "What should I buy if I had a budget of 11, and the precious thing {:cost 1, :value 20} were not available?"

;; She says "You should buy {:cost 6, :value 40}, and {:cost 3, :value 30}. That will cost you only 9, but get you 70!"

;; And you think to yourself: "Plus the precious thing, means cost 10 and value 90". Job done.

;; "But I will just check."

;; "Oh wise fairy, what should I buy had I a budget of 12 and the precious thing were not available?"

;; She says "You should buy {:cost 3, :value 30} {:cost 3, :value 21} {:cost 6, :value 40}, for a cost of 12 and a value of 91"

;; You note a certain awe-inspiring lack of smugness in the way she delivers this news.

;; Assuming you trust the fairy, you are now really done.

;; You either have to buy the precious thing, or not. If you do, then the best you can do is 90. If you don't, you can get 91.

;; Sadly, you put the precious thing back on the shelf and buy everything else.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "By the way," you ask the fairy. "How do you know the answers to the lesser questions?"

;; "I don't at first", she says, "I think it out the same way as you did, and then ask other fairies about the easier sub-problems.

;; "Would you like to join the large but finite society of weakly semi-good fairies?"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn knapsack [things budget]
  (cond (empty? things)  {:value 0 :purchases '()} ;; nothing to buy
        (zero?  budget)  {:value 0 :purchases '()} ;; no money left
        :else   (let [precious     (first things)
                      best-without (knapsack (rest things) budget)] ;; ask a fairy
                  (if (< budget (:cost precious))  ;; can we afford the precious thing?
                    best-without ;; if not we're done.
                    ;; otherwise ask a second fairy and cunningly compare their answers.
                    (let [sub-problem (knapsack (rest things) (- budget (:cost precious)))
                          best-with {:purchases (cons precious (:purchases sub-problem)) 
                                     :value (+ (:value precious) (:value sub-problem))}]
                      (max-key :value best-with best-without))))))


;; Ta-daa:
(knapsack things budget) ;-> {:purchases ({:cost 3, :value 30} {:cost 3, :value 21} {:cost 6, :value 40}), :value 91}
              
            

;; Here is a more complex problem        
(def stuff (doall (take 80 (repeatedly (fn[] {:cost (inc (rand-int 10)) :value (inc (rand-int 10))})))))
stuff ;-> ({:cost 6, :value 7} {:cost 6, :value 7} {:cost 9, :value 3} {:cost 8, :value 1} {:cost 5, :value 7} {:cost 7, :value 2} {:cost 9, :value 9} {:cost 7, :value 2} {:cost 10, :value 4} {:cost 8, :value 2} {:cost 2, :value 2} {:cost 7, :value 9} {:cost 5, :value 4} {:cost 3, :value 7} {:cost 5, :value 4} {:cost 3, :value 6} {:cost 3, :value 2} {:cost 9, :value 4} {:cost 1, :value 7} {:cost 10, :value 10} {:cost 4, :value 2} {:cost 6, :value 8} {:cost 3, :value 4} {:cost 4, :value 7} {:cost 1, :value 4} {:cost 6, :value 4} {:cost 6, :value 9} ...)

(knapsack stuff 1) ;-> {:purchases ({:cost 1, :value 7}), :value 7}
(knapsack stuff 2) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7}), :value 14}
(knapsack stuff 3) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 1, :value 5}), :value 19}
(knapsack stuff 4) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 5}), :value 23}
(knapsack stuff 5) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 5}), :value 27}
(knapsack stuff 6) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 1, :value 5} {:cost 3, :value 10}), :value 29}
(knapsack stuff 7) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 5} {:cost 3, :value 10}), :value 33}
(knapsack stuff 8) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 5} {:cost 3, :value 10}), :value 37}
(knapsack stuff 9) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 5} {:cost 3, :value 10} {:cost 2, :value 4}), :value 37}
(knapsack stuff 10) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 3, :value 8} {:cost 1, :value 4} {:cost 1, :value 5} {:cost 3, :value 10}), :value 41}
(knapsack stuff 11) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 7} {:cost 3, :value 8} {:cost 1, :value 4} {:cost 1, :value 5} {:cost 3, :value 10}), :value 45}

(time (knapsack stuff 1)) ;-> {:purchases ({:cost 1, :value 7}), :value 7}
"Elapsed time: 0.592813 msecs"
(time (knapsack stuff 2)) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7}), :value 14}
"Elapsed time: 1.224386 msecs"
(time (knapsack stuff 3)) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 1, :value 5}), :value 19}
"Elapsed time: 2.837093 msecs"
(time (knapsack stuff 4)) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 5}), :value 23}
"Elapsed time: 7.351429 msecs"
(time (knapsack stuff 5)) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 5}), :value 23}
"Elapsed time: 16.989729 msecs"
(time (knapsack stuff 5)) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 1, :value 4} {:cost 1, :value 5}), :value 23}



(defn ^:dynamic knapsack [things budget]
  (cond (empty? things)  {:value 0 :purchases '()} ;; nothing to buy
        (zero?  budget)  {:value 0 :purchases '()} ;; no money left
        :else   (let [precious     (first things)
                      best-without (knapsack (rest things) budget)] ;; ask a fairy
                  (if (< budget (:cost precious))  ;; can we afford the precious thing?
                    best-without ;; if not we're done.
                    ;; otherwise ask a second fairy and cunningly compare their answers.
                    (let [sub-problem (knapsack (rest things) (- budget (:cost precious)))
                          best-with {:purchases (cons precious (:purchases sub-problem)) 
                                     :value (+ (:value precious) (:value sub-problem))}]
                      (max-key :value best-with best-without))))))

(def knapsack (memoize knapsack))

(time (knapsack stuff 5))
"Elapsed time: 35.340664 msecs"
(time (knapsack stuff 6)) ;-> {:purchases ({:cost 1, :value 7} {:cost 1, :value 7} {:cost 1, :value 5} {:cost 3, :value 10}), :value 29}
(time (knapsack stuff 7))
"Elapsed time: 27.208697 msecs"
(time (knapsack stuff 10))
(time (knapsack stuff 100))
"Elapsed time: 283.731678 msecs"
(time (knapsack stuff 1000))
"Elapsed time: 1695.912931 msecs"
(time (knapsack stuff 10000))
"Elapsed time: 1087.614701 msecs"

(time (knapsack (take 80 (repeatedly (fn[] {:cost (inc (rand-int 10)) :value (inc (rand-int 10))}))) 80))

(def stuff (take 160 (repeatedly (fn[] {:cost (inc (rand-int 10)) :value (inc (rand-int 10))}))))
(time (knapsack (take 80 stuff) 160))


(def fib (memoize (fn [n] (if (< n 2) (bigint n) (+ (fib (dec n)) (fib (dec (dec n))))))))


(time (fib 10))
(time (fib 20))
(time (fib 30))
(time (fib 80))
(time (fib 160)) ;; stack overflow
(time (fib 320)) ;; stack overflow

(def factorial (memoize (fn [n] (if (< n 1) 0 (+ n (factorial (dec n)))))))

(factorial 1)
(factorial 80)
(factorial 160) ;; stack overflow
(factorial 320)
(factorial 3200)
(factorial 4000) ;; stack overflow here

;; Does anyone know what gives here?

;; I'm trying to memoize a rather complicated recursion, and it's blowing stack after not terribly many calls.

;; The simplest possible test case that manifests the behaviour seems to be:

(clojure-version) ;; "1.5.1"

(def gauss-recurse (fn [n] (if (< n 1) 0 (+ n (gauss-recurse (dec n))))))
(gauss-recurse 3500) ;-> 6126750

(def gauss-memoized (memoize (fn [n] (if (< n 1) 0 (+ n (gauss-memoized (dec n)))))))
(gauss-memoized 160)   ;; StackOverflowError   clojure.lang.RT.boundedLength (RT.java:1654)

























;; If (knapsack budget things) is the optimal purchase given all the things 
;; then we can say:

;; Should the (first things) be in the basket or not?

;; If it's not in the best basket, then the answer to 
;; (knapsack budget (rest things))
;; had better be the same answer, since if we're not going to buy it anyway, 
;; it shouldn't matter whether it's for sale or not.

;; Obviously, if we can't afford the (first things), then it can't be in the basket 

;; But if we can afford the first thing, then we can imagine what
;; would happen if we bought it and then worked out what the best
;; thing we could do with the remaining money is.

;; That would be:
;; (knapsack (- budget (:cost (first things))) (rest things))

;; So there are only two possibilities, and if we know the answers to
;; the two simpler problems then we can work out what our answer is by
;; comparing the results of the possibilities.

;; So if we knew what the answers to the two easier problems were, then we could 
;; work out what the answer to our problem would be.

;; Of course, if we've no budget, or there are no things for sale,
;; then we just don't buy anything.

(defn ^:dynamic knapsack [budget things]
  (cond (zero? budget) '()
        (empty? things) '()
        :else (let [basket1 (knapsack budget (rest things))
                    cost (:cost (first things))]
                (if (> cost budget) 
                  basket1
                  (let [basket2 (cons (first things) 
                                      (knapsack (- budget cost) (rest things)))]
                    (if (> (evaluate basket1) (evaluate basket2))
                      basket1
                      basket2))))))
        

;; behold
(knapsack budget things) ; ({:cost 3, :value 30} {:cost 3, :value 21} {:cost 6, :value 40})

;; and the maximal value is:
(evaluate (knapsack budget things)) ; -> 91

;; Which is the correct answer.

;; Now the eagle-eyed will notice that this is no better than
;; exhaustive search, since we make roughly two recursive calls per
;; available item, and so our time-to-solve is exponential:

(require 'clojure.tools.trace)
(clojure.tools.trace/dotrace [knapsack] (knapsack budget things))

;; TRACE t2106: (knapsack 12 ({:cost 1, :value 20} {:cost 3, :value 30} {:cost 3, :value 21} {:cost 6, :value 40}))
;; TRACE t2107: | (knapsack 12 ({:cost 3, :value 30} {:cost 3, :value 21} {:cost 6, :value 40}))
;; TRACE t2108: | | (knapsack 12 ({:cost 3, :value 21} {:cost 6, :value 40}))
;; TRACE t2109: | | | (knapsack 12 ({:cost 6, :value 40}))
;; TRACE t2110: | | | | (knapsack 12 ())
;; TRACE t2110: | | | | => ()
;; TRACE t2111: | | | | (knapsack 6 ())
;; TRACE t2111: | | | | => ()
;; TRACE t2109: | | | => ({:cost 6, :value 40})
;; TRACE t2112: | | | (knapsack 9 ({:cost 6, :value 40}))
;; TRACE t2113: | | | | (knapsack 9 ())
;; TRACE t2113: | | | | => ()
;; TRACE t2114: | | | | (knapsack 3 ())
;; TRACE t2114: | | | | => ()
;; TRACE t2112: | | | => ({:cost 6, :value 40})
;; TRACE t2108: | | => ({:cost 3, :value 21} {:cost 6, :value 40})
;; TRACE t2115: | | (knapsack 9 ({:cost 3, :value 21} {:cost 6, :value 40}))
;; TRACE t2116: | | | (knapsack 9 ({:cost 6, :value 40}))
;; TRACE t2117: | | | | (knapsack 9 ())
;; TRACE t2117: | | | | => ()
;; TRACE t2118: | | | | (knapsack 3 ())
;; TRACE t2118: | | | | => ()
;; TRACE t2116: | | | => ({:cost 6, :value 40})
;; TRACE t2119: | | | (knapsack 6 ({:cost 6, :value 40}))
;; TRACE t2120: | | | | (knapsack 6 ())
;; TRACE t2120: | | | | => ()
;; TRACE t2121: | | | | (knapsack 0 ())
;; TRACE t2121: | | | | => ()
;; TRACE t2119: | | | => ({:cost 6, :value 40})
;; TRACE t2115: | | => ({:cost 3, :value 21} {:cost 6, :value 40})
;; TRACE t2107: | => ({:cost 3, :value 30} {:cost 3, :value 21} {:cost 6, :value 40})
;; TRACE t2122: | (knapsack 11 ({:cost 3, :value 30} {:cost 3, :value 21} {:cost 6, :value 40}))
;; TRACE t2123: | | (knapsack 11 ({:cost 3, :value 21} {:cost 6, :value 40}))
;; TRACE t2124: | | | (knapsack 11 ({:cost 6, :value 40}))
;; TRACE t2125: | | | | (knapsack 11 ())
;; TRACE t2125: | | | | => ()
;; TRACE t2126: | | | | (knapsack 5 ())
;; TRACE t2126: | | | | => ()
;; TRACE t2124: | | | => ({:cost 6, :value 40})
;; TRACE t2127: | | | (knapsack 8 ({:cost 6, :value 40}))
;; TRACE t2128: | | | | (knapsack 8 ())
;; TRACE t2128: | | | | => ()
;; TRACE t2129: | | | | (knapsack 2 ())
;; TRACE t2129: | | | | => ()
;; TRACE t2127: | | | => ({:cost 6, :value 40})
;; TRACE t2123: | | => ({:cost 3, :value 21} {:cost 6, :value 40})
;; TRACE t2130: | | (knapsack 8 ({:cost 3, :value 21} {:cost 6, :value 40}))
;; TRACE t2131: | | | (knapsack 8 ({:cost 6, :value 40}))
;; TRACE t2132: | | | | (knapsack 8 ())
;; TRACE t2132: | | | | => ()
;; TRACE t2133: | | | | (knapsack 2 ())
;; TRACE t2133: | | | | => ()
;; TRACE t2131: | | | => ({:cost 6, :value 40})
;; TRACE t2134: | | | (knapsack 5 ({:cost 6, :value 40}))
;; TRACE t2135: | | | | (knapsack 5 ())
;; TRACE t2135: | | | | => ()
;; TRACE t2134: | | | => ()
;; TRACE t2130: | | => ({:cost 6, :value 40})
;; TRACE t2122: | => ({:cost 3, :value 30} {:cost 6, :value 40})
;; TRACE t2106: => ({:cost 3, :value 30} {:cost 3, :value 21} {:cost 6, :value 40})


;; But it is also possible to notice that there are only so many
;; subproblems as there are different combinations of lessened budgets
;; and shortened item lists.

;; So with one bound

(def ^:dynamic knapsack (memoize knapsack))

;; We are free:

(clojure.tools.trace/dotrace [knapsack] (knapsack budget things))

;; What was a huge tree recursion is by memoization made equivalent to
;; filling out values in a two dimensional array whose size is
;; proportional to the budget and the number of items.

;; That makes these sorts of computations feasible:
(knapsack 2000 (take 100 (repeatedly (fn[] {:cost (inc (rand-int 10)) :value (inc (rand-int 10))}))))

;; Unfortunately, however, this seems surprisingly prone to blowing stack:
;; Which makes me think that I've got the algorithm wrong.

(knapsack 80 (take 80 (repeatedly (fn[] {:cost (inc (rand-int 10)) :value (inc (rand-int 10))}))))
;; StackOverflowError   clojure.lang.RT.boundedLength (RT.java:1654)

(def stuff (doall (take 80 (repeatedly (fn[] {:cost (inc (rand-int 10)) :value (inc (rand-int 10))})))))

(clojure.tools.trace/dotrace [knapsack] (knapsack 1 stuff))
;; Appears to call itself about thirty times and then explode




;; If we wanted to solve a big version of this problem, we'd have to make an explicit iterative algorithm
;; going through a two-dimensional array


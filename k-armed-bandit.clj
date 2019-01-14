;; Reinforcement Learning : Exploration vs Exploitation : Multi-Armed Bandits

;; I'm reading the excellent:

;; Reinforcement Learning: An Introduction
;; by Richard S. Sutton and Andrew G. Barto

;; The book's website, on which is available a complete pdf, is here:
;; http://www.incompleteideas.net/book/the-book.html

;; In Chapter 2, they introduce multi-armed bandits as a simplified model problem 

;; On the basis that you don't understand anything you can't explain to a computer, I thought I'd code it up:

;; Here is a 2 armed bandit
(defn bandit [action]
  (case action
    :arms? [:right :left]
    :right (if (< (rand) 0.5) 4 0)
    :left (if (< (rand) 0.2) 5 0)
    :oops!!))

;; We can ask it how many arms it's got, and what they're called
(bandit :arms?) ; [:right :left]

;; And we can pull those arms. Rewards are variable.
(bandit :right) ; 4 ; 4 ; 4 ; 0 ; 0 ; 0 ; 0
(bandit :left) ; 5 ; 0 ; 0 ; 0 ; 5 ; 0 ; 5 ; 0

;; Once we pull an arm, we'll have an action/reward pair
(bandit :right) ; 4
;; the pair would be:
[:right 4]

(defn random-yank [bandit]
  (let [a (rand-nth (bandit :arms?))]
    [a (bandit a)]))

(random-yank bandit) ; [:right 4]

;; If we just pull arms at random we get an average reward of about 1.5
(defn average [seq] (/ (reduce + seq) (count seq)))

(float (average (map second (repeatedly 1000 #(random-yank bandit))))) ; 1.49

;; Since we can see the code for this particular bandit, we know that
;; the expected value of pulling the right arm is 2 (a half-chance of
;; a reward of 4) and the expected reward for the left arm is 0.2*5 = 1

;; So if we were seeking to maximize reward, we'd probably be best to pull the right arm all the time.

(float (average (take 10000 (map bandit (repeat :right))))) ; 1.9912 
(float (average (take 10000 (map bandit (repeat :left ))))) ; 0.985


;; The interesting question is, if we don't know how the bandit works, how should we design an algorithm that gets the most reward?
;; (Or at least do better than yanking arms at random!)

;; One thing our algorithm is going to have to do is keep some state to record what happens.
;; Let's start by recording the results of all pulls to date:

;; At first, we know nothing, so we can set up a table to represent that we know nothing
(defn initial-state [bandit]
  (into {} (for [k (bandit :arms?)] [k (list)])))

;; We haven't pulled either arm yet
(initial-state bandit) ; {:right (), :left ()}

;; When we get a new action reward/pair, we'll update our state
(defn update-state [state [action reward]]
  (update-in state [action] #(conj % reward)))

(update-state (initial-state bandit) [:right 4]) ; {:right (4), :left ()}


;; here are some examples of using update-state
(update-state {:right (), :left ()} [:right 2]) ; {:right (2), :left ()}
(reduce update-state {:right (), :left ()} [[:right 2] [:left 3]  [:right 4] [:right 5]]) ; {:right (5 4 2), :left (3)}
(reduce update-state
        (initial-state bandit)
        (repeatedly 10 #(random-yank bandit))) ; {:right (4 4 0 0 0), :left (0 0 0 0 5)}


;; Once we actually have some data, we can make estimates of the expected rewards

;; We'll use as our estimate of the value of an action the average value seen so far, or zero if we have no information

;; To help with this, a couple of utility functions:

;; average-list tells us the average value of a list of numbers, with a default value if the list is empty.
(defn average-list [lst default] (if (empty? lst) default (/ (reduce + lst) (count lst))))

(average-list (list 1 2 3 4 5) 0) ; 3
(average-list (list) 10) ; 10
(average-list (list 1) 2) ; 1
(average-list [] 100) ; 100

;; mapvals applies a function to every value in a map, returning a new map with the same keys
(defn mapvals [m f] (into {} (for [[k v] m] [k (f v)]))) 

;; examples
(mapvals {} inc) ; {}
(mapvals {:a 1} inc) ; {:a 2}
(mapvals {:a 1, :b 2} inc) ; {:a 2, :b 3}
(mapvals {:a 1, :b 2, :c 3} #(* % %)) ; {:a 1, :b 4, :c 9}


;; In the book, Q_t(a) is the current estimate (at time t)
;; Using the two functions, we can define our estimate so:

(defn Q [state] (mapvals state #(average-list % 0)))

;; examples
(Q '{:right (5 4 2), :left (3)}) ; {:right 11/3, :left 3}
(Q '{:right (5 4 2), :left ()}) ; {:right 11/3, :left 0}
(Q (initial-state bandit)) ; {:right 0, :left 0} 
(Q (update-state (initial-state bandit) [(rand-nth (bandit :arms?)) 2])) ; {:right 0, :left 2}

;; let's check that we get roughly what we expect in the long run
(Q (reduce update-state (initial-state bandit)
        (repeatedly 10000 #(random-yank bandit))))


;; If we have estimates of the value of each arm, then a good way to
;; use them is to pull the arm with the highest estimate.

;; This is called 'exploitation', as opposed to 'exploration', which
;; is when you try things you think may be suboptimal in order to get
;; information

;; The 'greedy' action is the one with the highest expected value. Of
;; course there may be more than one greedy action especially at
;; first.

;; To help with this, another utility function:

;; max-keys finds the keys with the highest value in a map, and returns a map with just these keys
(defn max-keys [m]
  (let [slist (reverse (sort-by second m)) 
        [_ max] (first slist)]
    (take-while #(= (second %) max) slist)))

;; examples
(max-keys {}) ; ()
(max-keys {1 0}) ; ([1 0])
(max-keys {1 0, 2 0}) ; ([2 0] [1 0])
(max-keys {1 0, 2 1}) ; ([2 1])
(max-keys {1 0, 2 1, 3 -1 , 4 -3, 5 2, 6 2}) ; ([6 2] [5 2])

;; if there is a tie for the greedy action, we can choose at random between the candidates
;; And so we can go from estimates to greedy action like this:
(defn greedy-action [estimates]
  (first (rand-nth (max-keys estimates))))

;; examples
(greedy-action '{:right 10, :left 3}) ; :right
(greedy-action '{:right 10, :left 3 :centre 20}) ; :centre
(greedy-action '{:right 10, :left 3 :centre 3}) ; :right
(greedy-action '{:right 3, :left 3 :centre 3}) ; :right ; :right ; :centre ; :right ; 

(greedy-action (Q '{:right (5 4 2), :left (3)})) ; :right
(greedy-action (Q '{:right (), :left (3)})) ; :left
(greedy-action (Q (initial-state bandit))) ; :left

;; after a lot of random pulls, the greedy action should reliably be the one with the highest expected payoff
(greedy-action (Q (reduce update-state (initial-state bandit)
        (repeatedly 10000 #(random-yank bandit))))) ; :right 

;; OK, so we have our stage set, a way of recording what's happened, and some helpful functions defined.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; we record it
(update-state (initial-state bandit) [:left 0]) ; {:right (), :left (0)}

;; and we have a new state
'{:right (), :left (0)}

;; new estimates
(Q '{:right (), :left (0)}) ; {:right 0, :left 0}

;; and again, we choose at random
(greedy-action (Q '{:right (), :left (0)})) ; :left

;; the bandit is not feeling very generous
(bandit :left) ; 0

(update-state '{:right (), :left (0)} [:left 0]) ; {:right (), :left (0 0)}

;; new state:
'{:right (), :left (0 0)}

;; new estimates
(Q '{:right (), :left (0 0)}) ; {:right 0, :left 0}

;; this time we choose :right
(greedy-action (Q '{:right (), :left (0 0)})) ; :right

;; and the bandit pays out! 
(bandit :right) ; 4

(update-state '{:right (), :left (0 0)} [:right 4]) ; {:right (4), :left (0 0)}

;; the greedy action will be :right now, because we have evidence that right is better.
(greedy-action (Q '{:right (4), :left (0 0)})) ; :right

;; You get the idea......

;; Let's automate that....

;; Given a state and a bandit, we decide an action and the bandit
;; responds, producing an action/reward pair, and a new state

(defn greedy-algorithm [bandit state]
  (let [action (greedy-action (Q state))
        reward (bandit action)]
    [[action reward] (update-state state [action reward])]))


(greedy-algorithm bandit (initial-state bandit)) ; [[:left 0] {:right (), :left (0)}]

;; To get something we can iterate:

(defn step [[[a r] state]]
  (greedy-algorithm bandit state))

(iterate step [ [:dummy :dummy] (initial-state bandit)])

;; ([[:dummy :dummy] {:right (), :left ()}]
;;  [[:left 5] {:right (), :left (5)}]
;;  [[:left 0] {:right (), :left (0 5)}]
;;  [[:left 0] {:right (), :left (0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 0 0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 0 0 0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 0 0 0 0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 0 0 0 0 0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 0 0 0 0 0 0 0 0 5)}]
;;  [[:left 5] {:right (), :left (5 0 0 0 0 0 0 0 0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 5 0 0 0 0 0 0 0 0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 5 0 0 0 0 0 0 0 0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 0 5 0 0 0 0 0 0 0 0 0 0 5)}]
;;  [[:left 0] {:right (), :left (0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 5)}]

;; In this case, the greedy algorithm happens to get a payout on its
;; first try, and decides that it will pull that arm for ever. It
;; never even tries the other arm.

;; Try again:

(iterate step [ [:dummy :dummy] (initial-state bandit)])
;;([[:dummy :dummy] {:right (), :left ()}]
;;  [[:right 0] {:right (0), :left ()}]
;;  [[:right 0] {:right (0 0), :left ()}]
;;  [[:left 0] {:right (0 0), :left (0)}]
;;  [[:right 4] {:right (4 0 0), :left (0)}]
;;  [[:right 4] {:right (4 4 0 0), :left (0)}]
;;  [[:right 4] {:right (4 4 4 0 0), :left (0)}]
;;  [[:right 4] {:right (4 4 4 4 0 0), :left (0)}]
;;  [[:right 4] {:right (4 4 4 4 4 0 0), :left (0)}]
;;  [[:right 4] {:right (4 4 4 4 4 4 0 0), :left (0)}]
;;  [[:right 0] {:right (0 4 4 4 4 4 4 0 0), :left (0)}]
;;  [[:right 0] {:right (0 0 4 4 4 4 4 4 0 0), :left (0)}]
;;  [[:right 4] {:right (4 0 0 4 4 4 4 4 4 0 0), :left (0)}]
;;  [[:right 0] {:right (0 4 0 0 4 4 4 4 4 4 0 0), :left (0)}]
;;  [[:right 4] {:right (4 0 4 0 0 4 4 4 4 4 4 0 0), :left (0)}]
;;  [[:right 0] {:right (0 4 0 4 0 0 4 4 4 4 4 4 0 0), :left (0)}]

;; In this case, it tried the right arm a couple of times, then had a
;; go with the left arm, then went back to the right arm, won a
;; payout, and then got hung up on pulling the right arm repeatedly.



;; We've got a couple of problems here!

;; First is that the algorithm has clearly got into a state where it
;; always pulls the left arm (in the first case), and the right
;; arm (in the second case).

;; It can't be doing the right thing in both cases.

;; Secondly the state is growing linearly, as the algorithm remembers
;; all previous results. That's giving us algorithmic complexity
;; problems and the calculation will get slower and slower, and
;; eventually run out of memory.














;; [org.clojure/clojure "1.4.0"]
;; [org.clojure/core.logic "0.8.1"]

(use 'clojure.core.logic)

;; Notes made while reading:
;; https://github.com/clojure/core.logic/wiki/A-Core.logic-Primer

;; Something like, if q is in [1 2 3], and q is in [2 3 4], what is q?

(run* [q]
            (membero q [1 2 3])
            (membero q [2 3 4])) 

;-> (2 3)

;; I wonder what the -o is for?

;; Apparently == is called unify here

(run* [q]
      (== q 1)) ;-> (1)

;; So this is 'What is q if q is 1?', or maybe 'what is q if q and 1 are unified?

;; But it's not just numbers, oh no:

(run* [q]
      (== q {:a 1 :b 2})) ;-> ({:a 1, :b 2})

;; This is more interesting:

(run* [q]
      (== {:a q :b 2} {:a 1 :b 2})) ;-> (1)

;; That appears to be 'What does q need to be for these two maps to be the same?'

;; I wonder if:

(run* [q p]
      (== {:a q :b 1} {:a 2 :b p})) ;-> ([2 1])

;; Oooh!


;; Another look at membero, although it doesn't do anything new:

(run* [q]
      (membero q [1 2 3])) ;-> (1 2 3)

(run* [q]
      (membero q [3 4 5])) ;-> (3 4 5)

(run* [q]
      (membero q [1 2 3])
      (membero q [3 4 5])) ;-> (3)

;; And now there's this fresh thing, which introduces a new variable which we don't get to see the answer of?

(run* [q]
      (fresh [a]
             (membero q [1 2 3])
             (membero q [3 4 5])
             (== a q))) ; -> (3)

;; So that's maybe 'What does q , which is in [3 4 5] have to be, in order that it be equal to another thing, should that other thing be in [1 2 3]?

;; We're order insensitive, apparently:
(run* [q]
      (fresh [a]
             (membero q [3 4 5])
             (== a q)
             (membero q [1 2 3]))) ;-> (3)


;; And there's a succeed operator, which wins:
(run* [q]
      succeed) ; -> (_0)

;; The (_0) means any value of q would have done.

;; Apparently _0 is just a symbol.

(type (first (run* [q] succeed))) ; -> clojure.lang.Symbol

;; Finally there's conde, which is some sort of OR

(run* [q]
      (conde 
       [succeed])) ; (_0)

;; That looks fairly straightforward, but what the hell is going on here?:

(run* [q]
      (conde 
       [succeed]
       [succeed])) ; (_0 _0)

;; Possible values of q are anything whatsoever or anything whatsoever?


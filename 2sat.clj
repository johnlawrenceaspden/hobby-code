;; 2-SAT and P = NP

;; The archetypal NP-complete problem is 3-SAT. Given a set of logical
;; triples that must be enforced, is there a satisfying allocation of
;; variables?

;; To warm up for this, let's try its tractable cousin 2-SAT

;; In 2-SAT, the clauses to be satisfied are all of form either:
;; (a must be true/false) or (b must be true/false)

;; So here's an example problem, with four variables and four clauses

;; Either x1 must be true or x2 must be true
;; Either x3 must be true or x4 must be true
;; Either x1 must be false or x3 must be false
;; Either x2 must be false or x4 must be false

;; A solution to the problem is either a satisfying assignment, or a
;; proof that no such assignment exists.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To make this trial problem more precise:

;; Represent an assignment as a vector of true, false values, then 
(def test-assignment [ true false true false ])

;; We can represent the first clause as:
(defn satisfies-1 [v] (or (nth v (dec 1) (nth v (dec 2)))))

(satisfies-1 test-assignment) ;-> true  ;; So far so good

;; The second clause as:
(defn satisfies-2 [v] (or (nth v (dec 3)) (nth v (dec 4))))

(satisfies-2 test-assignment) ;-> true  ;; Hooray

;; The third clause as:
(defn satisfies-3 [v] (or (not (nth v (dec 1))) (not (nth v (dec 3)))))

(satisfies-3 test-assignment) ;-> false  ;; Darn

;; And finally the fourth clause as:
(defn satisfies-4 [v] (or (not (nth v (dec 2))) (not (nth v (dec 4)))))

(satisfies-4 test-assignment) ;-> true   ;; Three out of four ain't bad

;; So actually we only failed one of the clauses with our guess.

;; What it means for the problem to be in NP is that given a proposed
;; satisfying assignment, we can check all these conditions in
;; polynomial time. (polynomial in n, the length of the input, here
;; take n to be the greater of the number of clauses and variables)

;; In fact it's obvious that it's linear in n. We need to check each
;; clause exactly once on a proposed solution.

(defn satisfies? [v]
  (and (satisfies-1 v)
       (satisfies-2 v)
       (satisfies-3 v)
       (satisfies-4 v)))

;; So 2-SAT is definitely in NP.

(satisfies? test-assignment) ;-> false

;; Given speedy checking of proposed solutions, we can find (or rule out) a satisfying solution by brute-force search:

(def possible-assignments 
  (for [i [true false]
        j [true false]
        k [true false]
        l [true false]] [i j k l]))

possible-assignments ;-> ([true true true true] [true true true false] [true true false true] [true true false false] [true false true true] [true false true false] [true false false true] [true false false false] [false true true true] [false true true false] [false true false true] [false true false false] [false false true true] [false false true false] [false false false true] [false false false false])

(count possible-assignments) ;-> 16  ;; Not too bad, we can just try them all:

(for [a possible-assignments :when (satisfies? a)] a) ;-> ([true false false true] [false true true false])

;; So we've found two possible satisfying assignments to the variables

(satisfies? [true false false true]) ;-> true
(satisfies? [false true true false]) ;-> true

;; One would have done. And if we'd found none, then that would have
;; shown the problem to be unsatisfiable, which is also a valid answer
;; to this 'decision problem'.

;; Because the space of possible solutions is exponential in the
;; number of variables, which is constrained by the size of the input,
;; the fact that 2-SAT is in NP and so each trial solution can be
;; assessed in polynomial time, guarantees us an exponential-time
;; solution to the problem.

;; The interesting question is 'can we do better than that?'

;; In the case of 2-SAT we can do much better. In fact it will turn
;; out that 2-SAT can be solved in (small) polynomial time, which
;; means that we can solve very large problems of this kind.

;; In the case of 3-SAT, almost certainly not. There's a vast class of
;; problems in NP which are all easier than 3 SAT, in the sense that
;; if we had a polynomial time 3-SAT algorithm then we'd automatically
;; also have polynomial time algorithms for this vast class, and
;; people have been trying to solve them for many years. And nobody
;; has ever come up with a polynomial time algorithm for any of them. 

;; This is known as the P=NP problem. It's thought not, but nobody knows.
;; It's probably the most important open maths problem there is.

;; In a randomly chosen permutation, what is the length of the longest cycle?

;; For permutations of (0 1 2), there's 1 perm where the longest cycle is 1
;; 3 where the longest cycle is 2, and 2 where the longest cycle is 3

;; (0 1 2) identity
;; (0 2 1) two-cycles  1<->2
;; (1 0 2)             1<->0
;; (2 1 0)             2<->0
;; (1 2 0) three-cycles  0->1->2->0   
;; (2 0 1)               0->2->1->0

;; So for the permutations of 3 things, there's
;; 1/6 chance that the longest cycle is one
;; 3/6 chance that the longest cycle is 2
;; 2/6 chance that the longest cycle is 3

;;What about the more general case?

;; Let p be a permutation expressed as a permutation of (range n)
;; (0 2 1) represents (0 1 2) -> (0 2 1), or 0->0, 1->2, 2->1, or in cycle notation: (1 2).

;; Make a function that takes an element of (range n) to its image under the permutation
(defn iterator [p] (vec p))

;; What is the orbit of a under the permutation p?
(defn get-cycle [p a]
  (let [it (iterator p)]
    (loop [cycle (list a), g (it a)]
        (if (= g a) (reverse cycle)
            (recur (cons g cycle) (it g) )))))

;; (get-cycle '(0 2 1) 0) (0)
;; (get-cycle '(0 2 1) 1) (0)

;; What is p in cycle notation?
(defn cycles [p]
  (loop [elements-to-try (set p) cycles '()]
    (if (empty? elements-to-try) cycles
        (let [cycle (get-cycle p (first elements-to-try))]
          (recur (apply disj elements-to-try cycle) (cons cycle cycles))))))

;; (cycles '(0 2 1)) -> ((1 2) (0))

;;What is the signature of p?
(defn signature [boxes]
  (sort (map count (cycles boxes))))

(signature '(0 2 1)) (1 2)
(signature '(0 1 2)) (1 1 1)
(signature '(1 2 0)) (3) 

;;What is the largest cycle of p?
(defn largest-cycle [p]
  (apply max (signature p)))

;;Finally, I get to use it in anger!
(defn factorial [n]
  (if (< n 2) n (* n (factorial (dec n)))))

;;Apply f to all the values of a map.
(defn mapmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

;;So, given an n, work out all the permuations of (range n), and their longest cycles
(defn longest-cycle-frequencies [n]
  (mapmap #(/ % (factorial n))
          (frequencies
           (map largest-cycle (clojure.contrib.combinatorics/permutations (range n))))))

;;Here are the frequencies for the first eight permutations
(map longest-cycle-frequencies (range 1 8))

;;({1 1}
;; {1 1/2, 2 1/2}
;; {1 1/6, 2 1/2, 3 1/3}
;; {1 1/24, 2 3/8, 3 1/3, 4 1/4}
;; {1 1/120, 2 5/24, 3 1/3, 4 1/4, 5 1/5}
;; {1 1/720, 2 5/48, 3 5/18, 4 1/4, 5 1/5, 6 1/6}
;; {1 1/5040, 2 11/240, 3 7/36, 4 1/4, 5 1/5, 6 1/6, 7 1/7})

;; It looks as though for cycles of length larger than half the permutation, the
;; chances are 1/n.  There's only one perm where all the cycles are length 1, so
;; it gets 1/(n!)  The numbers in between seem mysterious

;; If that assumption is correct, then the airmen's daily chance is
(- 1.0 (reduce + (map #(/ %) (range 51 100)))) ;;0.32182782068980476
;; about 32%, 

;; To shed some light on the frequencies of longest cycles, let's look at the frequencies of
;; the various signatures
(defn signature-frequencies [n]
  (mapmap #(/ % (factorial n))
          (frequencies
           (map signature (clojure.contrib.combinatorics/permutations (range n))))))

(map signature-frequencies (range 1 8)))

;; The groups on one and two elements are fairly simple
;; ({(1) 1}
;;  {(1 1) 1/2, (2) 1/2}

;; The group on three is more interesting
;;  {(1 1 1) 1/6, (1 2) 1/2, (3) 1/3}

;; How many different ways are there of writing 
;; (a) (b c)
;; where a,b,c are members of {1,2,3}?
;; as many as there are permutations!
;; (1) (2 3)
;; (1) (3 2)
;; (2) (3 1)
;; (2) (1 3)
;; (3) (1 2)
;; (3) (2 1)
;; But each permutation is represented here twice, (2 3) is the same cycle as (3 2)
;; Therefore there are 3!/2 elements with signature 2 1 in the permutation group of 3

;; What about
;; (a b c)?
;; Again, 6 ways, but now each permuation is triple-counted, since (1 2 3) is the same cycle as (2 3 1) and (3 1 2)



;; Here's the group on 4, with the probabilities of various element-signatures
;; {(1 1 1 1) 1/24, (1 1 2) 1/4, (1 3) 1/3, (2 2) 1/8, (4) 1/4}

;; How many different ways are there of writing
;; (a) (b) (c d)
;; where a,b,c,d are in {1, 2, 3, 4}
;; Now notice that
;; (1) (2) (3 4) == (1) (2) (4 3) == (2) (1) (3 4) == (2) (1) (3 4)
;; Again, 4! = 24 ways, and each element is counted four times, so we'd expect there to be 6 distinct ones

;; What about
;; (a b) (c d)
;; 24 ways, and each element counted 2x2x2=8 times, so there are three.

;; Theorize that the number of ways of representing an element with the signature
(1 1 2 2 5 7 7 7)
;; Would be (1x1x2x2x5x7x7x7)x(2!x2!x3!)
;; product of cycle lengths x factorials of sizes of sets of equal lengths

(defn counting-multiplicity[signature]
  (*
   (reduce * (map factorial (vals (frequencies signature))))
   (reduce * signature)))

(counting-multiplicity '(1 1 2 2 5 7 7 7))

;; Let's look at the group on five, and try to calculate the probabilities of each signature
(map #(/ %) (map counting-multiplicity '((1 1 1 1 1), (1 1 1 2), (1 1 3) (1 2 2) (1 4) (2 3) (5))))

;; (1/120 1/12 1/6 1/8 1/4 1/6 1/5)

;;  {(1 1 1 1 1) 1/120,
;;   (1 1 1 2) 1/12,
;;   (1 1 3) 1/6,
;;   (1 2 2) 1/8,
;;   (1 4) 1/4,
;;   (2 3) 1/6,
;;   (5) 1/5}

;; Looks plausible! Now we need something to generate possible signatures of a permutation of n things
;; the "partitions of n".

;; Let's look at some partitions:

;; The partitions of one are   '( (1) )

;; The partitions of two are   '( (2), (1 1) )
;; NB the partitions of two with no element higher that one are '((1 1))
;; The partitions of two with no ones in are '((2))

;; The partitions of three are '( (3), (2 1), (1 1 1) )
;; NB the partitions of three with no element higher that one are '((1 1))
;; And the partitions of three with no element higher than two are '((2 1) (1 1 1))
;; And the partitions of three with no ones in are '(3)

;; The partitions of four are  '( (4), (3 1), (3 1) (2 2) (2 1 1), (2 1 1) (1 2 1), (1 1 2) (1 1 1 1) )

;; The trick here is to see that a partition of n
;; must have a smallest number m. In which case it must be a partition of m-n
;; with the same smallest number, plus an extra m.

(partitions 3)
(partitions 3 1)
(1 + (partitions 2 1))+(partitions 3 2)
(partitions 2 1) = (1 +(partitions 1 1)) + (

(partitions 6 1)
(concat
 (map (partial cons 1) (partitions 5 1))
 (partitions 6 2))

(partitions 5 1)




(defn partitions [n, m]
  (cond (< n m) '()
        (= n n) (list (list n))
        (< n 0) '()
        :else (concat (map (partial cons m) (partitions (- n m) m ))
                      (partitions n (inc m)))))

(partitions 2 2)
(concat (map (partial cons 1) (partitions 2 1))
        (partitions 2 3)

(partitions 1 1)

;;  {(3 3) 1/18,
;;   (2 2 2) 1/48,
;;   (1 1 1 1 2) 1/48,
;;   (1 2 3) 1/6,
;;   (1 1 1 1 1 1) 1/720,
;;   (1 1 2 2) 1/16,
;;   (2 4) 1/8,
;;   (1 1 4) 1/8,
;;   (1 1 1 3) 1/18,
;;   (6) 1/6,
;;   (1 5) 1/5}

;;  {(1 1 1 1 1 1 1) 1/5040,
;;   (1 1 1 2 2) 1/48,
;;   (1 3 3) 1/18,
;;   (3 4) 1/12,
;;   (2 2 3) 1/24,
;;   (1 1 1 1 3) 1/72,
;;   (1 2 4) 1/8,
;;   (1 2 2 2) 1/48,
;;   (1 1 1 1 1 2) 1/240,
;;   (1 1 2 3) 1/12,
;;   (2 5) 1/10,
;;   (1 1 5) 1/10,
;;   (1 1 1 4) 1/24,
;;   (7) 1/7,
;;   (1 6) 1/6})

;; A Very Gentle Introduction to Information Theory : Part V

;; Guessing the Entropy Function

;; Once again, I'll keep some code from the earlier parts, without explanation
;; Just in case anyone except me is still interested, I've added doc strings.

;; Probability distributions

(defn combine-keywords
  "(combine-keywords :a :b) -> :ab"
  [& a] (keyword (apply str (mapcat #(drop 1 (str %)) a))))

(defn combine-distributions
  "combine a number of independent probability distributions
  (combine-distributions {:H 1 :T 1} {:A 1 :B 2 :C 1}) -> {:HA 1, :HB 2, :HC 1, :TA 1, :TB 2, :TC 1}"
  ([P] P)
  ([P1 P2]
     (into {}
           (for [[s1 p1] P1
                 [s2 p2] P2]
             [(combine-keywords s1 s2) (* p1 p2)])))
  ([P1 P2 & Plist] (reduce combine-distributions (cons P1 (cons P2 Plist)))))

;; Poor man's Priority Queue

(defn pqmake
  "create a poor man's priority-queue out of a sorted map of lists
  (pqmake {:HA 1 :HB 2 :HC 1}) -> {1 (:HC :HA), 2 (:HB)}"
  [P] (reduce pqadd (sorted-map) P))

(defn pqadd
  "add a new thing to a poor man's priority-queue
  (pqadd '{1 (:HC :HA), 2 (:HB)} [:new 0]) -> {0 (:new), 1 (:HC :HA), 2 (:HB)}"
  [m [k p]] (assoc m p (cons k (m p))))

(defn pqpop
  "take the first item from a poor man's priority-queue, return the priority, the thing, and the new queue without the thing popped.
  (pqpop '{0 (:new), 1 (:HC :HA), 2 (:HB)}) -> [0 :new {1 (:HC :HA), 2 (:HB)}]"
  [m]
  (let [[k vlist] (first m)]
    [k (first vlist)
    (if (empty? (rest vlist))
      (dissoc m k)
      (assoc m k (rest vlist)))]))


;; Huffman encoding

(defn pqcombine 
  "pop the first two items from a poor man's priority-queue, make a vector out of their values, and add that back in with the new priority being the sum of the old priorities. This is the basic operation for Huffman coding. Returns the modified queue.
  (pqcombine '{ 1 (:a) 2 (:b :c :d) 3 (:e)}) -> {2 (:c :d), 3 ([:a :b] :e)}"
  [m]
  (let [[pa a pop1] (pqpop m)
        [pb b pop2] (pqpop pop1)]
    (pqadd pop2 [[a b] (+ pa pb)])))

(defn make-code-tree
  "Takes a probability distribution, returns a code tree which represents its Huffman code.
  (make-code-tree {:H 1 :T 1 :E 2}) -> [[:T :H] :E]"
  [P]
  (let [Pheap (pqmake P)
        combinations (iterate pqcombine Pheap)]
    (second (pqpop (nth combinations (dec (count P)))))))

(defn make-code
  "Get the actual code from a code tree. Blows stack if tree too deep.
      (make-code [[:T :H] :E]) -> {:T (1 1), :H (1 0), :E (0)}"
  ([code-tree]
     (let [helper (fn helper [prefix code-tree]
                    (if (keyword? code-tree) (list prefix code-tree)
                        (concat (helper (cons 1 prefix) (first code-tree))
                                (helper (cons 0 prefix) (second code-tree)))))]
       (into {} (map (fn[[c s]][s (reverse c)]) (partition 2 (helper '() code-tree)))))))

;; This should work, but change the 2000 to 3000 and kerboom!
;; Not fixing because it's unlikely to be a problem in practice.
;; (make-code (make-code-tree
;;   (apply sorted-map (take 2000
;;     (interleave (map #(keyword (str %)) (range)) (iterate #(* 2 %) 1))))))

(defn huffman-code [P]
  "A Huffman code for the distribution P
  (huffman-code {:A 1 :B 2 :C 4}) -> {:A (1 1), :B (1 0), :C (0)}"
  (make-code (make-code-tree P)))

(defn expected-code-length
  "Given a distribution and a code, calculate the expected length of a transmission
  (expected-code-length {:H 1 :T 1} {:H '(1) :T '(0 0) :E '(0 1)}) -> 3/2"
  [P code]
  (let [cd (for [s (keys P)] [(P s) (code s)])]
    (/ (reduce + (for [[k v] cd] (* k (count v))))
       (reduce + (map first cd)))))


(defn bits [n]
  "How many bits to represent n alternatives? Fractions allowed! Also know as log2."
  (/ (Math/log n)(Math/log 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; So far, we've seen that when transmitting the results of fair coin tosses over a
;; binary channel, huffman encoding does 'the right thing', but that it's not very helpful.

(def fair-coin {:H 1 :T 1})
(huffman-code fair-coin) ; {:T (1), :H (0)}
(huffman-code (combine-distributions fair-coin fair-coin)) ; {:HT (1 1), :HH (1 0), :TT (0 1), :TH (0 0)}

;; However, if we've got a fair die
(def fair-die {:1 1 :2 1 :3 1 :4 1 :5 1 :6 1})
(huffman-code fair-die) ; {:6 (1 1), :5 (1 0), :2 (0 1 1), :1 (0 1 0), :4 (0 0 1), :3 (0 0 0)}

;; Then by shortening some transmissions, it can help us do better than the
;; three bits per die we'd naively use to code the results
(float (expected-code-length fair-die (huffman-code fair-die))) ; 2.6666667

;; Indeed, if we allow ourselves to combine rolls before transmission then it can squeeze even more redundancy out
(float (let [n 4 
             fair-dice (apply combine-distributions (repeat n fair-die))]
         (/ (expected-code-length fair-dice (huffman-code fair-dice)) 4))) ; 2.6049383

;; There's clearly a law of diminishing returns here, but the final value we can
;; squeeze down to appears to be related to the base 2 logarithm of the number
;; of faces of the die.

(bits 6) ; 2.584962500721156

;; What sorts of values do we get from non-uniform distributions?
(defn huffman-entropy [P n]
  (let [Pn (apply combine-distributions (repeat n P))]
    (float (/ (expected-code-length Pn (huffman-code Pn)) n))))

(def twoD6 {:2 1 :3 2 :4 3 :5 4 :6 5 :7 6 :8 5 :9 4 :10 3 :11 2 :12 1})

(huffman-entropy twoD6 1) ; 3.3055556
(huffman-entropy twoD6 2) ; 3.2901235
(huffman-entropy twoD6 3) ; 3.2839506
(huffman-entropy twoD6 4) ; 3.2820425

;; When we're throwing 2D6 and only reporting the sum of the dice, we're throwing some information away.

;; Suppose we'd like to send that as well.

;; If we're sending 2 or 12, then there's nothing more to say. The roll must have been 1,1 or 6,6.
;; When we're sending 3 or 11, then there are two possibilities for each, 1,2 or 2,1, and 5,6 or 6,5
;; These are both equally likely, so that's like sending a fair coin toss in addition, every time we send 11 or 3
;; Similarly 4 and 10 can be any one of three pairs of rolls, etc.

;; So we have a 2/36 chance of not needing to send any extra information, and a 6/36 chance of having to send an extra bit, and an 8/36 chance of having to send an extra uniform choice from 3, etc.

;; We've already guessed that a uniform choice from 3 takes (bits 3) of bandwidth on average.

;; If we were to send the extra information for the two D6, then that would add up to

(+ (* 4/36 (bits 2)) (* 6/36 (bits 3)) (* 8/36 (bits 4)) (* 10/36 (bits 5)) (* 6/36 (bits 6))) ; 1.8955230821535418

;; But if we send all that, we might as well have sent the two separate rolls 

(bits 36) ; 5.169925001442312

;; If we subtract the information that we throw away by adding the numbers from the total information in the two die rolls,
;; then we get

( - (bits 36) (/ (reduce + (map * '(4 6 8 10 6) (map bits '(2 3 4 5 6)))) 36)) ; 3.2744019192887706

;; Compare:
(huffman-entropy twoD6 5) ; 3.2799246

;; Which looks like a bit too much of a coincidence to be a coincidence.

;; Let's try some other variations of this idea.

;; What about if we toss a fair coin, and then if it's heads we toss it again?
;; That would give us this distribution.
{:T 2 :HT 1 :HH 1}
;; We'd expect the entropy here to be that of a fair coin toss (1 bit), plus half the time, another bit from the second toss.
;; So the entropy should be 1.5 bits.

(huffman-entropy {:T 2 :HT 1 :HH 1} 1) ; 1.5
(huffman-entropy {:T 2 :HT 1 :HH 1} 2) ; 1.5
(huffman-entropy {:T 2 :HT 1 :HH 1} 3) ; 1.5

;; Shannon defined entropy thus:

(defn shannon-entropy [P]
  (let [odds (map second P)
        total (reduce + odds)
        bits-aliased (/ (reduce + (map * odds (map bits odds))) total)]
        (- (bits total) bits-aliased)))

;; And this magic formula describes all the numbers we've seen so far, which fell naturally
;; out of the idea of using shorter codes for more common occurrences.

;; It's pretty much forced on you as 'the cost of transmitting the results of a random process'
;; once you've thought of Morse Code.

(def fair-coin {:H 1 :T 1})
(def unfair-coin {:H 3 :T 1})
(def unfair-triples (combine-distributions unfair-coin unfair-coin unfair-coin))

(shannon-entropy fair-coin) ; 1.0
(shannon-entropy fair-die) ; 2.584962500721156
(shannon-entropy {:T 2 :HT 1 :HH 1}) ; 1.5
(shannon-entropy twoD6) ; 3.2744019192887706
(shannon-entropy unfair-coin) ; 0.8112781244591327
(shannon-entropy unfair-triples) ; 2.4338343733773984
(/ (shannon-entropy unfair-triples) 3) ; 0.8112781244591328























(def triple-code-tree (make-code-tree unfair-triples))
(def triple-code (make-code triple-code-tree))


;; We don't need to estimate the cost of a code by generating a long random stream and transmitting it.

;; Given a probability distribution and a code, we can just calculate the expected cost:

;; We make a distribution over the transmitted symbols


;; e.g.
(code-distribution unfair-triples triple-code)
;;([27 (0)] [9 (1 1 1)] [9 (1 1 0)] [3 (1 0 0 1 0)] [9 (1 0 1)] [3 (1 0 0 0 1)] [3 (1 0 0 0 0)] [1 (1 0 0 1 1)])

;; And from that, it's easy to calculate the expected length of a sequence


;; So the expected cost per symbol is:
(expected-code-length unfair-triples triple-code) ;79/32
;; or per coin-toss:
(float ( / 79/32 3)) ; 0.8229167

;; So we can get the noise out of our table:

(defn cost-for-n-code [ P n ]
     (let [Pn (apply combine-distributions (repeat n P))
           code (make-code (make-code-tree Pn))]
       (float (/ (expected-code-length Pn code) n))))

(cost-for-n-code unfair-coin 1) ; 1.0
(cost-for-n-code unfair-coin 2) ; 0.84375
(cost-for-n-code unfair-coin 3) ; 0.8229167
(cost-for-n-code unfair-coin 4) ; 0.8183594
(cost-for-n-code unfair-coin 5) ; 0.81777346
(cost-for-n-code unfair-coin 6) ; 0.8186849
(cost-for-n-code unfair-coin 7) ; 0.81685966
(cost-for-n-code unfair-coin 8) ; 0.81575775
(cost-for-n-code unfair-coin 9) ; 0.81493336
(cost-for-n-code unfair-coin 10) ; 0.8141917
(cost-for-n-code unfair-coin 11) ; 0.8137328
(cost-for-n-code unfair-coin 12) ; 0.81351095

;; It looks like something is converging, although the convergence isn't monotonic. 
;; I'm now revising my estimate of the cost of sending the results of a 1:3 process to be about 0.813

;; But we don't know whether that's the limit of the coding process, or a property of the 1:3 distribution,
;; or whether it's in some way specific to transmitting over a binary channel.

;; Let's look at some other distributions.

;; For the fair coin distribution, huffman coding triples doesn't help at all.
(cost-for-n-code fair-coin 1) ; 1.0
(cost-for-n-code fair-coin 2) ; 1.0
(cost-for-n-code fair-coin 3) ; 1.0
(cost-for-n-code fair-coin 4) ; 1.0

;; But for an even choice between three things, it does:
(def triad {:A 1 :B 1 :C 1})

(cost-for-n-code triad 1) ; 1.6666666
(cost-for-n-code triad 2) ; 1.6111112
(cost-for-n-code triad 3) ; 1.6049383
(cost-for-n-code triad 4) ; 1.6049383
(cost-for-n-code triad 5) ; 1.5893004
(cost-for-n-code triad 6) ; 1.5992227
(cost-for-n-code triad 7) ; 1.5895878
(cost-for-n-code triad 8) ; 1.5939262



;; For a choice between four things, it makes no difference
(def quad {:A 1 :B 1 :C 1 :D 1})

(cost-for-n-code quad 1) ; 2.0
(cost-for-n-code quad 2) ; 2.0
(cost-for-n-code quad 3) ; 2.0
(cost-for-n-code quad 4) ; 2.0
(cost-for-n-code quad 5) ; 2.0

;; For five it's a good thing to do
(def quint {:A 1 :B 1 :C 1 :D 1 :E 1})

(cost-for-n-code quint 1) ; 2.4
(cost-for-n-code quint 2) ; 2.36
(cost-for-n-code quint 3) ; 2.3253334
(cost-for-n-code quint 4) ; 2.3404
(cost-for-n-code quint 5) ; 2.337856
(cost-for-n-code quint 6) ; 2.3252373


;; And again, for the next power of two, no difference.
(def octet {:A 1 :B 1 :C 1 :D 1 :E 1 :F 1 :G 1 :H 1})

(cost-for-n-code octet 1) ; 3.0
(cost-for-n-code octet 2) ; 3.0
(cost-for-n-code octet 3) ; 3.0

;; I think that we might have guessed that it would take three bits to decide between
;; eight equally likely things, and two bits for four things, but what about the other numbers?

;; If 8 = 2*2*2 -> 3, and 4 = 2*2 -> 2, and 2 -> 1, what's the easiest pattern we can fit to that?

(defn bits [n] (/ (Math/log n)(Math/log 2))) ;; Also known as log2

(map bits (range 2 10)) ; (1.0 1.5849625007211563 2.0 2.321928094887362 2.584962500721156 2.807354922057604 3.0 3.1699250014423126)

;; So let's make a prediction
(def sextet {:A 1 :B 1 :C 1 :D 1 :E 1 :F 1})
(bits 6) ; 2.584962500721156

(cost-for-n-code sextet 1) ; 2.6666667
(cost-for-n-code sextet 2) ; 2.6111112
(cost-for-n-code sextet 3) ; 2.6049383
(cost-for-n-code sextet 4) ; 2.6049383
(cost-for-n-code sextet 5) ; 2.5893004
(cost-for-n-code sextet 6) ; 2.5992227


;; It looks as though the cost of coding an even distribution using huffman
;; encoding of runs is pretty close to being the logarithm (to base 2) of the number of symbols.



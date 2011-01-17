;; A Very Gentle Introduction to Information Theory : Part IV

;; Entropy and Huffman Coding

;; Once again, I'll keep some code from the earlier parts, without explanation

;; Probability distributions

(defn combine-keywords [& a] (keyword (apply str (mapcat #(drop 1 (str %)) a))))

(defn combine-distributions
  ([P] P)
  ([P1 P2]
     (into {}
           (for [[s1 p1] P1
                 [s2 p2] P2]
             [(combine-keywords s1 s2) (* p1 p2)])))
  ([P1 P2 & Plist] (reduce combine-distributions (cons P1 (cons P2 Plist)))))

;; Coding and Decoding

(defn decoder
  ([code-tree stream] (decoder code-tree code-tree stream))
  ([current-code-tree code-tree stream]
     (lazy-seq
        (if (keyword? current-code-tree)
          (cons current-code-tree (decoder code-tree code-tree stream))
          (if-let [stream (seq stream)]
            (if (= (first stream) 1)
              (decoder (first current-code-tree)  code-tree (rest stream))
              (decoder (second current-code-tree) code-tree (rest stream))))))))

(defn encoder [code stream] (mapcat code stream))

(defn make-encoder [code]  (fn [s] (encoder code s)))
(defn make-decoder [code-tree] (fn[s] (decoder code-tree s)))

;; Huffman encoding

(defn symbols [prefix code-tree]
  (if (keyword? code-tree) (list prefix code-tree)
      (concat (symbols (cons 1 prefix) (first code-tree))
              (symbols (cons 0 prefix) (second code-tree)))))

(defn make-code [code-tree]
  (into {} (map (fn[[c s]][s (reverse c)]) (partition 2 (symbols '() code-tree)))))

;; The original make-code-tree was very slow, because it re-sorted the list every
;; iteration. We can speed it up considerably by using a priority queue instead
;; of sorting the list every iteration. Clojure doesn't have one built in, so
;; here's a poor man's version built out of a sorted map of lists.

(defn madd [m [k p]] (assoc m p (cons k (m p))))

(defn mpop [m]
  (let [[k vlist] (first m)]
    [k (first vlist)
    (if (empty? (rest vlist))
      (dissoc m k)
      (assoc m k (rest vlist)))]))

(defn mcombine [m]
  (let [[pa a pop1] (mpop m)
        [pb b pop2] (mpop pop1)]
    (madd pop2 [[a b] (+ pa pb)])))

(defn make-code-tree [P]
  (second (mpop
          (nth (iterate mcombine (reduce madd (sorted-map) P)) (dec (count P))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def fair-coin {:H 1 :T 1})
(def unfair-coin {:H 3 :T 1})
(def unfair-triples (combine-distributions unfair-coin unfair-coin unfair-coin))

(def triple-code-tree (make-code-tree unfair-triples))
(def triple-code (make-code triple-code-tree))


;; We don't need to estimate the cost of a code by generating a long random stream and transmitting it.

;; Given a probability distribution and a code, we can just calculate the expected cost:

;; We make a distribution over the transmitted symbols
(defn code-distribution [P code]  (for [s (keys P)] [(P s) (code s)]))

;; e.g.
(code-distribution unfair-triples triple-code)
;;([27 (0)] [9 (1 1 1)] [9 (1 1 0)] [3 (1 0 0 1 0)] [9 (1 0 1)] [3 (1 0 0 0 1)] [3 (1 0 0 0 0)] [1 (1 0 0 1 1)])

;; And from that, it's easy to calculate the expected length of a sequence
(defn expected-code-length [P code]
  (let [cd (code-distribution P code)]
    (/ (reduce + (for [[k v] cd] (* k (count v))))
       (reduce + (map first cd)))))

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



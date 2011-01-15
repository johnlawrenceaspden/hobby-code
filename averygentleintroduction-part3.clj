;; A Very Gentle Introduction to Information Theory : Part III

;; Entropy and Huffman Coding

;; Once again, I'll keep some code from the first two parts, without explanation

(defn random-stream [P]
  (let [pseq (vec (mapcat (fn[[k v]](repeat v k )) P))]
    (for [i (range)] (rand-nth pseq))))

(defn cost [encoder decoder message]
  (let [coded (encoder message)]
    (if (= (decoder coded) message) (count coded) :fail)))

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

(defn combine-keywords [& a] (keyword (apply str (mapcat #(drop 1 (str %)) a))))
(defn split-keyword [a] (map #(keyword (str %)) (drop 1 (str a))))

(defn make-combination-encoder [code n]
  (fn [s] (encoder code (map #(apply combine-keywords %) (partition n s)))))

(defn make-combination-decoder [code-tree]
  (fn [s] (mapcat split-keyword (decoder code-tree s))))

;; So far we've looked at three probability distributions:
(def fair-coin {:H 1 :T 1})
(def unfair-coin {:H 3 :T 1})
(def unfair-pairs {:HH 9, :HT 3, :TH 3, :TT 1})

;; And two codes:
(def fair-code-tree [:H :T])
(def fair-code {:H '(1) :T '(0)})
     
(def unfair-code-tree [ :HH [ :HT [ :TH :TT]]])
(def unfair-code {:HH '(1) :HT '(0 1) :TH '(0 0 1) :TT '(0 0 0)})

;; We should add a fourth probability distribution to represent pairs of fair coin toss results
(def fair-pairs {:HH 1 :HT 1 :TH 1 :TT 1})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We found that

(defn estimate-cost [P encoder decoder]
  (let [n 100000
        c (cost encoder decoder (take n (random-stream P)))]
    (if (number? c) (float (/ c n)) c)))

;; Using the best code we can think of for the fair coin resulted in a transmission cost of 1 (£/symbol)
(estimate-cost fair-coin   (make-encoder fair-code) (make-decoder fair-code-tree)) ; 1.0
;; And that that was also the cost for the unfair coin with this code:
(estimate-cost unfair-coin (make-encoder fair-code) (make-decoder fair-code-tree)) ; 1.0

;; But that we could come up with a code for pairs of coin tosses
;; which did substantially better for pairs of unfair coin tosses
(estimate-cost unfair-pairs (make-encoder unfair-code) (make-decoder unfair-code-tree)) ; 1.68338
;; but substantially worse for pairs of fair coin tosses
(estimate-cost fair-pairs   (make-encoder unfair-code) (make-decoder unfair-code-tree)) ; 2.24722
;; remember that that's the transmission cost per symbol, and that each symbol represents two coin tosses

;; In case you think there's any sleight of hand going on there, here's how we'd use the pairs code to transmit
;; the original unpaired streams
(estimate-cost unfair-coin  (make-combination-encoder unfair-code 2) (make-combination-decoder unfair-code-tree)) ; 0.84561
(estimate-cost fair-coin    (make-combination-encoder unfair-code 2) (make-combination-decoder unfair-code-tree)) ; 1.12407
;; Notice that the costs here are per-toss, showing that the unfair code is actually an improvement 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now it seems that, if we can send the results of unfair-coin more efficiently
;; by considering {:HH 9, :HT 3, :TH 3, :TT 1}, the distribution of pairs of
;; tosses, then we should have a look at the distribution of triples, and work out a code for that:

(defn combine-distributions
  ([P] P)
  ([P1 P2]
     (into {}
           (for [[s1 p1] P1
                 [s2 p2] P2]
             [(combine-keywords s1 s2) (* p1 p2)])))
  ([P1 P2 & Plist] (reduce combine-distributions (cons P1 (cons P2 Plist)))))

(def unfair-triples (combine-distributions unfair-coin unfair-coin unfair-coin))

;; unfair-triples is {:HHH 27, :HHT 9, :HTH 9, :HTT 3, :THH 9, :THT 3, :TTH 3, :TTT 1}

;; Now how should we work out a code for this distribution?

;; Huffman tells us that we should combine the two lowest probability events
;; so that
{:HHH 27, :HHT 9, :HTH 9, :HTT 3, :THH 9, :THT 3, :TTH 3, :TTT 1}
;; goes to
{:HHH 27, :HHT 9, :HTH 9, :HTT 3, :THH 9, :THT 3, {:TTH 3, :TTT 1} 4}
;; and then do it again, so that
{:HHH 27, :HHT 9, :HTH 9, :HTT 3, :THH 9, :THT 3, {:TTH 3, :TTT 1} 4}
;; goes to
{:HHH 27, :HHT 9, :HTH 9, :HTT 3, :THH 9, {:THT 3, {:TTH 3, :TTT 1} 4} 7}
;; and so on .....

(defn huffman-combine [P]
  (let [plist (sort-by second P)
        newelement (into {} (take 2 plist))]
    (into {} (cons [newelement (reduce + (vals newelement))] (drop 2 plist)))))

(nth (iterate huffman-combine unfair-triples) (dec (dec (count unfair-triples))))
;; {{{:HHT 9, :HTH 9} 18, {:THH 9, {{:TTT 1, :HTT 3} 4, {:THT 3, :TTH 3} 6} 10} 19} 37, :HHH 27}

;; At the end, we get a sort of nested binary probability distribution
;; You could think of this as being a way to generate the triples by tossing strangely biased coins!

;; From that, we can generate our code tree directly by just throwing away the numbers
(require 'clojure.walk)
(defn make-code-tree [P]
  (clojure.walk/postwalk #(if (map? %) (into[] (map first  %)) %)
                         (nth (iterate huffman-combine P) (dec (dec (count P))))))

(def triple-code-tree (make-code-tree unfair-triples))
;;[[[:HHT :HTH] [:THH [[:TTT :HTT] [:THT :TTH]]]] :HHH]

;; If we have the decoder, then we can use it to generate the coder!

(defn symbols [prefix code-tree]
  (if (keyword? code-tree) (list prefix code-tree)
      (concat (symbols (cons 1 prefix) (first code-tree))
              (symbols (cons 0 prefix) (second code-tree)))))

(defn make-code [code-tree]
  (into {} (map (fn[[c s]][s (reverse c)]) (partition 2 (symbols '() code-tree)))))

(def triple-code (make-code triple-code-tree))
;; {:HHT (0 0 0), :HTH (0 0 1), :THH (0 1 0), :TTT (0 1 1 0 0), :HTT (0 1 1 0 1), :THT (0 1 1 1 0), :TTH (0 1 1 1 1), :HHH (1)}

;; Let's see how this does
(estimate-cost unfair-triples (make-encoder triple-code) (make-decoder triple-code-tree)) ; 2.4615

;; £2.46 per symbol, or 0.82p per toss

;; So while going from single tosses to pairs allowed us to go from 1->0.85, going from pairs to triples only allowed us to get from 0.85->0.82.

;; Is there an end to this process?

(defn bit-rate [P n]
  (let [Pn (apply combine-distributions (repeat n P))
        tree (make-code-tree Pn)]
    (/ (estimate-cost Pn (make-encoder (make-code tree)) (make-decoder tree)) n)))

(bit-rate unfair-coin 1) ; 1.0
(bit-rate unfair-coin 2) ; 0.844435
(bit-rate unfair-coin 3) ; 0.82466
(bit-rate unfair-coin 4) ; 0.8196275
(bit-rate unfair-coin 5) ; 0.818912
(bit-rate unfair-coin 6) ; 0.81896996
(bit-rate unfair-coin 7) ; 0.8166514
(bit-rate unfair-coin 8) ; 0.815995
(bit-rate unfair-coin 9) ; 0.81352335
(bit-rate unfair-coin 10) ; 0.81462896
(bit-rate unfair-coin 11) ; 0.8137691

;; To me, this is at least suggestive that there might be something fundamental
;; about a cost of 82p to transmit the result of a 3:1 random result over a
;; binary channel.
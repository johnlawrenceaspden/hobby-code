;; A Very Gentle Introduction to Information Theory : Part IV

;; Entropy and Huffman Coding

;; Once again, I'll keep some code from the earlier parts, without explanation

;; Probability distributions

(defn random-stream [P]
  (let [pseq (vec (mapcat (fn[[k v]](repeat v k )) P))]
    (for [i (range)] (rand-nth pseq))))

(defn combine-keywords [& a] (keyword (apply str (mapcat #(drop 1 (str %)) a))))
(defn split-keyword [a] (map #(keyword (str %)) (drop 1 (str a))))

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

;; Generating Huffman codes for probability distributions

(defn huffman-combine [P]
  (let [plist (sort-by second P)
        newelement (into {} (take 2 plist))]
    (into {} (cons [newelement (reduce + (vals newelement))] (drop 2 plist)))))

(require 'clojure.walk)
(defn make-code-tree [P]
  (clojure.walk/postwalk #(if (map? %) (into[] (map first  %)) %)
                         (nth (iterate huffman-combine P) (dec (dec (count P))))))

(defn symbols [prefix code-tree]
  (if (keyword? code-tree) (list prefix code-tree)
      (concat (symbols (cons 1 prefix) (first code-tree))
              (symbols (cons 0 prefix) (second code-tree)))))

(defn make-code [code-tree]
  (into {} (map (fn[[c s]][s (reverse c)]) (partition 2 (symbols '() code-tree)))))

;; Send a message, verify that it gets decoded correctly, report the cost or :fail

(defn cost [encoder decoder message]
  (let [coded (encoder message)]
    (if (= (decoder coded) message) (count coded) :fail)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def fair-coin {:H 1 :T 1})
(def unfair-coin {:H 3 :T 1})
(def unfair-triples (combine-distributions unfair-coin unfair-coin unfair-coin))

(def triple-code-tree (make-code-tree unfair-triples))
(def triple-code (make-code triple-code-tree))


;; We don't need to estimate the cost of a code by generating a long random stream and transmitting it.

;; Given a probability distribution and a code, we can just calculate the average cost:

;; A distribution, e.g. unfair-triples:
{:HHH 27, :HHT 9, :HTH 9, :HTT 3, :THH 9, :THT 3, :TTH 3, :TTT 1}
;; And a code: e.g. triple-code
'{:HHT (1 1 1), :HTH (1 1 0), :THH (1 0 1), :TTT (1 0 0 1 1), :HTT (1 0 0 1 0), :THT (1 0 0 0 1), :TTH (1 0 0 0 0), :HHH (0)}

;; Give us, as long as the code can encode every possible output of the distribution, a distribution of transmitted codes:
(defn code-distribution [P code]  (for [s (keys P)] [(P s) (code s)]))

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

;; It looks like something is converging. 
;; I'm now revising my estimate of the cost of sending the results of a 1:3 process to be about 0.813

;; But we don't know whether that's the limit of the coding process, or a quality of the 1:3 distribution,
;; or whether it's in some way specific to transmitting over a binary channel.

;; Let's look at some other distributions.

;; But first, make-code-tree is rather slow
(defn huffman-combine [P]
  (let [plist (sort-by second P)
        newelement (into {} (take 2 plist))]
    (into {} (cons [newelement (reduce + (vals newelement))] (drop 2 plist)))))

(require 'clojure.walk)
(defn make-code-tree [P]
  (clojure.walk/postwalk #(if (map? %) (into[] (map first  %)) %)
                         (nth (iterate huffman-combine P) (dec (dec (count P))))))

;; And it's obvious why. The size of the distribution to be partitioned is
;; doubling, every time, which there's no avoiding, but for a size n
;; distribution, the huffman-combine function is run (n-2) times, and its
;; algorithm is to sort the distribution, then add a new element, then turn it
;; back into a hash-map. Any of those steps might itself be order n, so we're
;; squaring a number that's growing exponentially.

;; The proper data structure to use when you just want the two smallest values
;; from a list is called a priority queue, or heap. I don't think clojure's got
;; one, so we'll see what we can do with a simple sorted list, which might be a
;; little slower but still an improvement.
(map float (map / '(0.3 5 3 19 48 121 213 495 1325 3470 8817) '(1 2 4 8 16 32 64 128 256 512 1024)))
;; actually it's really not that good. Wonder what's going on here.
(map float (map / '(0.3 0.7 1.46 8.92 19.93 62 115 426 1763 7482) '(1 2 4 8 16 32 64 128 256 512 1024)))

(def a (combine-distributions sextet sextet sextet sextet sextet))

(defn priority-compare [a b]
  (if (= (first a) (first b))
    (.compareTo (str (second a)) (str (second b)))
    (< (first a) (first b))))

(def b (apply sorted-set-by priority-compare (map vec (map reverse a))))

(defn huffman-combine-sorted-set[s]
  (let [[a b :as c] (take 2 s)
        d (disj s a)
        e (disj d b)]
    (conj e [ (reduce + (map first c)) (vec (map second c)) ])))
    
(defn fast-make-code-tree [P]
  (let [b (apply sorted-set-by priority-compare (map vec (map reverse P)))
        tree (nth (iterate huffman-combine-sorted-set b) (dec (count b)))]
    (second (first (vec tree)))))

(fast-make-code-tree (apply combine-distributions (repeat 1 sextet)))
        
 



(def triad {:A 1 :B 1 :C 1})

(cost-for-n-code triad 1) ; 1.6666666
(cost-for-n-code triad 2) ; 1.6111112
(cost-for-n-code triad 3) ; 1.6049383
(cost-for-n-code triad 4) ; 1.6049383
(cost-for-n-code triad 5) ; 1.5893004
(cost-for-n-code triad 6) ; 1.5992227

(def quad {:A 1 :B 1 :C 1 :D 1})

(cost-for-n-code quad 1) ; 2.0
(cost-for-n-code quad 2) ; 2.0
(cost-for-n-code quad 3) ; 2.0
(cost-for-n-code quad 4) ; 2.0
2.5893004(cost-for-n-code quad 5) ; 2.0

(def quint {:A 1 :B 1 :C 1 :D 1 :E 1})

(cost-for-n-code quint 1) ; 2.4
(cost-for-n-code quint 2) ; 2.36
(cost-for-n-code quint 3) ; 2.3253334
(cost-for-n-code quint 4) ; 2.3404

(def octet {:A 1 :B 1 :C 1 :D 1 :E 1 :F 1 :G 1 :H 1})

(cost-for-n-code octet 1) ; 3.0
(cost-for-n-code octet 2) ; 3.0
(cost-for-n-code octet 3) ; 3.0


;; I think that we might have guessed that it would take three bits to decide between
;; eight equally likely things, and two bits for four things, but what about the other numbers?

;; If 8 = 2*2*2 -> 3, and 4 = 2*2 -> 2, and 2 -> 1, what's the easiest pattern we can fit to that?

(defn bits [n] (/ (Math/log n)(Math/log 2))) ;; Also known as log2

(map bits (range 2 10)) ; (1.0 1.5849625007211563 2.0 2.321928094887362 2.584962500721156 2.807354922057604 3.0 3.1699250014423126)

(def sextet {:A 1 :B 1 :C 1 :D 1 :E 1 :F 1})
(bits 6) ;; 2.584962500721156

(cost-for-n-code sextet 1) ; 2.6666667
(cost-for-n-code sextet 2) ; 2.6111112
(cost-for-n-code sextet 3) ; 2.6049383
(cost-for-n-code sextet 4) ; 2.6049383
(cost-for-n-code sextet 5) ; 


;; It looks as though the 



;; A Very Gentle Introduction to Information Theory : Part II

;; Entropy and Huffman Coding

;; Here's the essential code from part I, which I'm not going to explain again:

(defn random-stream [P]
  (let [pseq (vec (mapcat (fn[[k v]](repeat v k )) P))]
    (for [i (range)] (rand-nth pseq))))

(defn cost [encoder decoder message]
  (let [coded (encoder message)]
    (if (= (decoder coded) message) (count coded) :fail)))

(def unfair-pairs {:HH 9, :HT 3, :TH 3, :TT 1})

;; We're trying to transmit the output of the random process represented by:

(def stream (random-stream unfair-pairs))

(take 20 stream) ;(:HH :HH :HH :HH :HH :HH :HH :HH :HT :HH :HH :TH :HH :HH :HH :TT :HH :HH :HT :HT)

;; And we're using the code HH -> 1, HT ->01 TH->001, TT-> 000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Our code seems to have something of a tree structure

;; 1-> :HH
;; 0-> 1 -> :HT
;;     0 -> 1 -> :TH
;;          0 -> :TT


;; Let's see if we can find some way of expressing that, so that we don't have to hand-code a decoder
;; for every different code.

(def code-tree [ :HH [ :HT [ :TH :TT]]])

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

(decoder code-tree '(0 1 1 0 1 0 1 1 0 0 0)) ;(:HT :HH :HT :HT :HH :TT)
  
;; A general encoder, by comparison, is fairly straightforward:

(def code {:HH '(1) :HT '(0 1) :TH '(0 0 1) :TT '(0 0 0)})

(defn encoder [code stream]
  (mapcat code stream))

(take 20 (encoder code stream)) ;(1 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 0 0)

;; Trying the two together:

(take 20  (decoder code-tree (encoder code stream))) ;(:HH :HH :HH :HH :HH :HH :HH :HH :HT :HH :HH :TH :HH :HH :HH :TT :HH :HH :HT :HT)

;; And finally:

(defn make-encoder [code]  (fn [s] (encoder code s)))
(defn make-decoder [code-tree] (fn[s] (decoder code-tree s)))

(cost (make-encoder code) (make-decoder code-tree) (take 10000 stream)) ; £16992

;; It costs us £16992 to send 10000 symbols.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We can use our code to send the output from the original biased coin very easily

(def unfair-coin {:H 3 :T 1})

(def unfair-stream (random-stream unfair-coin))

(take 20 unfair-stream) ; (:H :H :H :H :H :H :T :H :H :H :H :H :H :H :T :H :H :H :H :T)

(defn combine-keywords [& a] (keyword (apply str (mapcat #(drop 1 (str %)) a))))
(defn split-keyword [a] (map #(keyword (str %)) (drop 1 (str a))))


(defn make-combination-encoder [code n]
  (fn [s] (encoder code (map #(apply combine-keywords %) (partition n s)))))

(defn make-combination-decoder [code-tree]
  (fn [s] (mapcat split-keyword (decoder code-tree s))))

(cost (make-combination-encoder code 2) (make-combination-decoder code-tree) (take 10000 unfair-stream)) ; £8460

;; So our method of coding for {:HH 9, :HT 3, :TH 3, :TT 1} has given us a method of coding for {:H 3, :T 1}
;; which is 16% more efficient than the obvious one.

;; What if we try it on the output from the fair coin?

(def fair-stream (random-stream {:H 1 :T 1}))

(cost (make-combination-encoder code 2) (make-combination-decoder code-tree) (take 10000 fair-stream)) ; £ 11257

;; Using this code on the output from an unbiased coin actually makes it more expensive to transmit!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To recap:

;; We can transmit the output from a series of coin tosses, or other random
;; processes, down a binary channel, if we choose a code.

;; The code can be trivial, like :H -> 0 :T -> 1,
;; or it can be complex, like :HH -> 1, :HT -> 01, :TH ->001, :TT -> 000

;; Different codes can result in different costs of transmission for the outputs
;; of different processes

;; So far, we've seen costs of £1/symbol for the fair coin with the trivial code
;; or £1.12/symbol with the more complex code

;; And we've seen costs of £1/symbol and £0.84/symbol for the unfair coin with
;; the trivial and complex code respectively.

;; It seems that choosing the right code can make transmission cheaper, and
;; choosing the wrong code can make it more expensive.




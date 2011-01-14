;; A Very Gentle Introduction to Information Theory : Part I

;; Entropy and Huffman Coding

;; What is the 'information content' of a random process?

;; We might think about a Victorian bookmaker transmitting horse racing results
;; over an expensive telegraph connection. A more modern example would be
;; streaming files over an internet connection.

;; To think about the essence of these things, let us choose very simple models
;; for both the random process and the channel over which the message is to be
;; sent.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up a model problem

;; Our communications channel will be a device over which we can transmit either
;; 0 or 1, paying a cost of £1 for every symbol.

;; Our random number generators will make arbitrary symbols from a known set,
;; one a second, with simple integer odds.

;; One example might produce the symbols A B and C, with A:B:C in a 2:1:1 ratio.

;; Sample output from this particular random number generator might be
;; BACAAAABCAACBCAACAAABAABCBAAACBC....

;; And our challenge is to send a message down our channel that will allow our
;; friend at the other end to reconstruct the random stream.

;; We'd be interested in minimizing the cost, but not at the expense of accuracy.

;; If the random number generator is someone repeatedly tossing a fair coin, then we can say:

(def fair-coin {:H 1 :T 1})

(defn random-stream [P]
  (let [pseq (vec (mapcat (fn[[k v]](repeat v k )) P))]
    (for [i (range)] (rand-nth pseq))))

;; And if the fair coin were to come up with:

(def coin-stream (random-stream fair-coin))

(take 20 coin-stream) ; (:T :H :H :T :H :H :T :H :T :T :T :T :H :T :T :T :T :H :T :T)

;; We might want to transmit the results by sending 1 for tails and 0 for heads

(defn coin-coder [sq] (map #(if (= % :T) 0 1) sq))

(take 20 (coin-coder (random-stream fair-coin))) ; (1 0 0 1 0 0 0 0 1 0 0 1 0 0 1 1 1 0 1 0)

;; Our friend on the other end, with whom we have agreed this scheme, might decode like this:

(defn coin-decoder [sq] (map #(if (= % 0) :T :H) sq))

(take 20 (coin-decoder (coin-coder coin-stream))) ; (:T :H :H :T :H :H :T :H :T
                                                  ; :T :T :T :H :T :T :T :T :H
                                                  ; :T :T)

;; And finally, the world might judge our scheme thus:

(defn cost [encoder decoder message]
  (let [coded (encoder message)]
    (if (= (decoder coded) message) (count coded) :fail)))

(cost coin-coder coin-decoder (take 200000 coin-stream)) ; £200000

;; Under this scheme, our message gets through accurately, and we have spent
;; £200000 to transmit 200000 symbols.

;; If anyone can think of a better encoding scheme, please let me know. Until I
;; see a counter-example, I'll take the information content of a fair coin under
;; these conditions to be £1/symbol.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A 'less random' random generator

(def unfair-coin {:H 3 :T 1})

(def unfair-stream (random-stream unfair-coin))

(take 20 unfair-stream) ; (:T :H :H :T :H :H :H :H :H :H :H :H :H :H :T :H :H :H
                        ; :H :H)

;; It seems as though there might be less information in the unfair coin tosses.

;; Why do I say this?

;; Well, memory is a kind of channel. You put something in, and later you get
;; something out, and it costs you something to hold the memory.

;; And the 20 tosses of the unfair coin above seem easier to remember than 20
;; tosses of the fair coin would be.

;; It's quite hard to make this intuition precise, though. After all, all HT
;; sequences are still possible.  But it does seem as though we should be able
;; to send these sorts of streams through our channel for less cost than the
;; fair coin streams, ON AVERAGE.

;; To think about how that might work, let's consider what the stream above
;; looks like when split into pairs.

(take 20 (partition 2 unfair-stream)) ;;((:T :H) (:H :T) (:H :H) (:H :H) (:H :H)
                                      ;;(:H :H) (:H :H) (:T :H) (:H :H) (:H :H)
                                      ;;(:T :H) (:H :H) (:H :H) (:T :H) (:H :H)
                                      ;;(:T :H) (:H :T) (:T :H) (:T :H) (:T :H))

(frequencies (take 16000 (partition 2 unfair-stream)))
;; {(:T :H) 2981, (:H :T) 3083, (:H :H) 8964, (:T :T) 972}

;; Because the frequencies of the underlying coin are distorted 3:1, the
;; frequencies of the pairs are even more distorted, it looks like 1:3:3:9

;; A little bird is telling me that the next random number generator we try to
;; code should be:

(def unfair-pairs {:HH 9, :HT 3, :TH 3, :TT 1})

(def unfair-pair-stream (random-stream unfair-pairs))

(take 15 unfair-pair-stream ) ; (:HH :TH :HH :HH :HT :HT :HH :HT :HH :TH :TH :HH
                              ; :HH :TH :HT)

;; How might we go about encoding this to send through our channel?

;; If we do it the obvious way:

(defn pair-coder [sq] (mapcat #(case % :HH '(0 0) :HT '(0 1) :TH '(1 0) :TT '(1 1)) sq))

(take 20 (pair-coder unfair-pair-stream)) ; (0 0 1 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 1 0)

(defn pair-decoder [sq]
  (map #(case % '(0 0) :HH '(0 1) :HT '(1 0) :TH '(1 1) :TT) (partition 2 sq)))

(take 20 (pair-decoder (pair-coder unfair-pair-stream)))
     ; (:HH :TH :HH :HH :HT :HT :HH :HT :HH :TH :TH :HH :HH :TH :HT :HH :HH :HT :HH :TH)

(cost pair-coder pair-decoder (take 200000 unfair-pair-stream)) ; £400000

;; Then we can see that it costs £2 per symbol to transmit the data now, which
;; sounds about right, since we made each symbol out of a pair of the previous
;; symbols, so we could use them as a cheaper transmission method if they cost
;; any less.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sneaking more data down the telegraph

;; As Samuel Morse knew, when transmitting in binary, we should use shorter
;; sequences for more common letters.

;; Let's try HH -> 1, HT ->01 TH->001, TT-> 000

(defn variable-length-pair-coder [sq] (mapcat #(case % :HH '(1) :HT '(0 1) :TH '(0 0 1) :TT '(0 0 0)) sq))

(take 20 (variable-length-pair-coder unfair-pair-stream)) ; (1 0 0 1 1 1 0 1 0 1 1 0 1 1 0 0 1 0 0 1)

;; decoding this is trickier

(defn variable-length-pair-decoder [sq]
  (lazy-seq
   (if-let [sq (seq sq)]
     (if (= (first sq) 1)
       (cons :HH (variable-length-pair-decoder (rest sq)))
       (if-let [sq2 (seq (rest sq))]
         (if (= (first sq2) 1)
           (cons :HT (variable-length-pair-decoder (rest sq2)))
           (if-let [sq3 (seq (rest sq2))]
             (if (= (first sq3) 1)
               (cons :TH (variable-length-pair-decoder (rest sq3)))
               (cons :TT (variable-length-pair-decoder (rest sq3)))))))))))

;; but it can be done:
(take 20 (variable-length-pair-decoder (variable-length-pair-coder unfair-pair-stream)))
;;(:HH :TH :HH :HH :HT :HT :HH :HT :HH :TH :TH :HH :HH :TH :HT :HH :HH :HT :HH :TH)

;; and it saves on the number of transmitted symbols:
(cost variable-length-pair-coder variable-length-pair-decoder (take 200000 unfair-pair-stream)) ; £337179

;; For this particular stream, with this particular code, we're now only paying
;; £ 1.69 per symbol, as opposed to £2 per symbol for the naive code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


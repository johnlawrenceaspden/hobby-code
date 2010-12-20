;; Information Theory, Inference, and Learning Algorithms
;; Exercise 1.4

;; A binary symmetric channel of noise f flips each bit with probability f

(defn flip[bit] (if (= bit 0) 1 0))

(defn flip-rand[f bit] (if (< (rand) f) (flip bit) bit))

(defn corrupt[f sq] (map #(flip-rand f %) sq))

(defn corrupt[f sq]
  (lazy-seq
   (when-let [sq (seq sq)]
     (cons (if (< (rand) f) (flip (first sq)) (first sq))
           (corrupt f (rest sq))))))


(defn binary-symmetric-channel[f] (fn[sq] (corrupt f sq)))

(defn estimate-bit-error [coder decoder n]
;  (with-precision 15
;    (bigdec
     (let [channel (binary-symmetric-channel 0.1)
           message-in (repeat n (rand-int 2))
           tx (coder message-in)
           rx (channel tx)
           message-out (decoder rx)
           ferrs (frequencies (map bit-xor message-in message-out))]
       (/ (get ferrs 1 0) (+ (get ferrs 0 0) (get ferrs 1 0)))))




;; repetition codes just repeat each bit n times
(defn repetition-code [n sq]
  (lazy-seq
   (when-let [sq (seq sq)]
     (concat (repeat n (first sq)) (repetition-code n (rest sq))))))

(defn repetition-code [n sq] (mapcat #(repeat n %) sq))


(defn repetition-decode [n sq]
  (lazy-seq
   (when-let [sq (seq sq)]
     (let [ssq (take n sq)
           d (first (last (sort-by second (frequencies ssq))))]
       (cons d (repetition-decode n (drop n sq)))))))

(defn repetition-decode [n sq]
  (map ffirst
       (map #(sort-by second > %)
            (map frequencies
                 (partition n sq)))))

(defn repetition-coder   [n] (fn [sq] (repetition-code n sq)))
(defn repetition-decoder [n] (fn [sq] (repetition-decode n sq)))

;; these keep coming in handy. uncomment if needed
;; (defn factorial [n] (if (< n 2) 1N (* n (factorial (dec n)))))
;; (defn choose [ n r ]  (/ (factorial n) (factorial r) (factorial (- n r))))


;; Hamming's 7 4 code
(defn parity [ & sq] (reduce bit-xor sq))


(defn hamming-7-4 [[a b c d]]
  (let [e (parity a b c)
        f (parity b c d)
        g (parity c d a)]
    [a b c d e f g]))

(defn hamming-7-4-syndrome [[a b c d e f g]]
  [(parity a b c e) (parity b c d f) (parity c d a g)])

(defn syndrome-to-bit [a b c]
  (case [a b c]
        [1 0 1] 0
        [1 1 0] 1
        [1 1 1] 2
        [0 1 1] 3
        'ok))
(defn hamming-7-4-correct [[a b c d e f g :as v]]
  (def-let [syn (apply syndrome-to-bit (hamming-7-4-syndrome v))]
    (if (= syn 'ok) [a b c d]
        (vec (concat (take syn v) (cons (flip (nth v syn)) (drop (inc syn) (list a b c d))))))))



(defn hamming-7-4-encoder [sq]
  (mapcat hamming-7-4 (partition 4 sq)))

(defn hamming-7-4-decoder [sq]
  (mapcat hamming-7-4-correct (partition 7 sq)))


;; Tests and sanity checks
            
(use 'clojure.test)
(deftest corruption
  (is (= ((binary-symmetric-channel 1)   (repeat 10 1)) (repeat 10 0)))
  (is (= ((binary-symmetric-channel 0)   (repeat 10 1)) (repeat 10 1)))
  (is (= (count (frequencies ((binary-symmetric-channel 0.5) (repeat 10 1)))) 2))
  
  (is (= ((repetition-coder 3) (repeat 10 1)) (repeat 30 1)))
  (is (= ((repetition-decoder 3) (repeat 30 0)) (repeat 10 0)))
  (is (= ((repetition-coder 7) (repeat 10 1)) (repeat 70 1)))
  (is (= ((repetition-decoder 7) (repeat 70 0)) (repeat 10 0)))
  
  (is (< 0.01 (estimate-bit-error (repetition-coder 3) (repetition-decoder 3) 1000) 0.1))
  (is (< (estimate-bit-error (repetition-coder 9) (repetition-decoder 9) 1000) 0.003))

  (is (= (hamming-7-4 '(0 0 0 0)) [0 0 0 0 0 0 0]))
  (is (= (hamming-7-4 '(0 0 0 1)) [0 0 0 1 0 1 1]))
  (is (= (hamming-7-4 '(0 0 1 0)) [0 0 1 0 1 1 1]))
  (is (= (hamming-7-4 '(0 0 1 1)) [0 0 1 1 1 0 0]))
  (is (= (hamming-7-4 '(0 1 0 0)) [0 1 0 0 1 1 0]))
  (is (= (hamming-7-4 '(0 1 0 1)) [0 1 0 1 1 0 1]))
  (is (= (hamming-7-4 '(0 1 1 0)) [0 1 1 0 0 0 1]))
  (is (= (hamming-7-4 '(0 1 1 1)) [0 1 1 1 0 1 0]))
  (is (= (hamming-7-4 '(1 0 0 0)) [1 0 0 0 1 0 1]))
  (is (= (hamming-7-4 '(1 0 0 1)) [1 0 0 1 1 1 0]))
  (is (= (hamming-7-4 '(1 0 1 0)) [1 0 1 0 0 1 0]))

  
  (is (= (hamming-7-4-correct [0 0 0 0 0 0 0]) [0 0 0 0]))
  (is (= (hamming-7-4-correct [0 0 0 0 0 0 1]) [0 0 0 0]))
  (is (= (hamming-7-4-correct [0 0 0 0 0 1 0]) [0 0 0 0]))
  (is (= (hamming-7-4-correct [0 0 0 0 1 0 0]) [0 0 0 0]))
  (is (= (hamming-7-4-correct [0 0 0 1 0 0 0]) [0 0 0 0]))
  (is (= (hamming-7-4-correct [0 0 1 0 0 0 0]) [0 0 0 0]))
  (is (= (hamming-7-4-correct [0 1 0 0 0 0 0]) [0 0 0 0]))
  (is (= (hamming-7-4-correct [1 0 0 0 0 0 0]) [0 0 0 0]))
  
  (is (= (hamming-7-4-syndrome [0 0 0 0 0 0 0]) [0 0 0]))
  (is (= (hamming-7-4-syndrome [1 0 0 0 0 0 0]) [1 0 1]))
  (is (= (hamming-7-4-syndrome [0 1 0 0 0 0 0]) [1 1 0]))
  (is (= (hamming-7-4-syndrome [0 0 1 0 0 0 0]) [1 1 1]))
  (is (= (hamming-7-4-syndrome [0 0 0 1 0 0 0]) [0 1 1]))
  (is (= (hamming-7-4-syndrome [0 0 0 0 1 0 0]) [1 0 0]))
  (is (= (hamming-7-4-syndrome [0 0 0 0 0 1 0]) [0 1 0]))
  (is (= (hamming-7-4-syndrome [0 0 0 0 0 0 1]) [0 0 1]))

  (is (= (syndrome-to-bit 0 0 0) 'ok))
  (is (= (syndrome-to-bit 1 1 1) 2))
  (is (= (syndrome-to-bit 0 1 1) 3))

  (is (= (parity 1 1 1) 1))

  (is   (= (apply syndrome-to-bit (hamming-7-4-syndrome [0 0 0 0 0 0 0])) 'ok))
  (is   (= (apply syndrome-to-bit (hamming-7-4-syndrome [0 0 0 1 0 0 0])) 3))

  (is (= (hamming-7-4-encoder '(0 0 0 0 0 1 0 1 1 1 1 1))
         '(0 0 0 0 0 0 0 0 1 0 1 1 0 1 1 1 1 1 1 1 1)))
  (is (= (hamming-7-4-decoder (hamming-7-4-encoder '(0 1 0 1 0 0 1 0 1 0 1 1 )))
         '(0 1 0 1 0 0 1 0 1 0 1 1 )))






  )
(run-tests)


(def errrates
  (map
   #(estimate-bit-error (repetition-coder %) (repetition-decoder %) 10000)
   '(3 5 7 9 11))) ; (0.0272M 0.0088M 0.0026M 0.001M 0.0004M)

;; Theory predicts 0.36 = 4*0.1*0.9 asymptotically
(map float (map / (drop 1 errrates) errrates)) ;; (0.372 0.290 0.37 0.5)


(float (estimate-bit-error hamming-7-4-encoder hamming-7-4-decoder 10000))

(use 'clojure.contrib.monads)

(domonad identity-m
         [a 2
          b (inc a)]
         (* a b ))

(domonad maybe-m
         [a 2
          b (inc a)]
         (* a b))

(defn identity-bind [value function]
  (function value))

(identity-bind 2 (fn [a]
                   (identity-bind (inc a) (fn [b]
                                            (* a b)))))

(defn maybe-bind [value function]
  (if (nil? value) nil
      (function value)))

(maybe-bind 2 (fn [a]
                (maybe-bind (inc a) (fn [b]
                                      (* a b)))))


(defn word->number [word]
  (condp = word
    "one" 1
    "two" 2
    "three" 3
    "four" 4
    "five" 5
    nil))

(map word->number '("one" "two" "three" "four" "five" "six"))

(defn add-words [aw bw]
  (let [a (word->number aw)
        b (word->number bw)]
    (+ a b)))

(add-words "one" "two")
(add-words "one" "six")        

(defn maybe-add-words [aw bw]
  (let [a (word->number aw)]
    (if (nil? a) nil
        (let [b (word->number bw)]
          (if (nil? b) nil
              (+ a b))))))

(maybe-add-words "one" "two")
(maybe-add-words "one" "six")
(maybe-add-words "six" "one")
(maybe-add-words "six" "six")

(maybe-bind (word->number "one") (fn [a]
                    (maybe-bind (word->number "two") (fn [b]
                                        (+ a b)))))

(maybe-bind (word->number "one") (fn [a]
                    (maybe-bind (word->number "six") (fn [b]
                                        (+ a b)))))

(domonad maybe-m
         [a (word->number "one")
          b (word->number "two")]
         (+ a b))

(domonad maybe-m
         [a (word->number "one")
          b (word->number "six")]
         (+ a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(for [a (range 5)
      b (range a)]
  (* a b))

(domonad sequence-m
         [a (range 5)
          b (range a)]
         (* a b))

(defn almost-sequence-bind [sequence function]
  (map function sequence))

(almost-sequence-bind (range 5) (fn [a]
                           (almost-sequence-bind (range a) (fn [b]
                                                      (* a b)))))

(defn sequence-bind [sequence function]
  (apply concat (map function sequence)))

(defn sequence-result [value]
  (list value))

(sequence-bind (range 5) (fn [a]
                           (sequence-bind (range a) (fn [b]
                                                      (sequence-result (* a b))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(with-monad sequence-m
   (defn ntuples [n xs]
      (m-seq (replicate n xs))))

(ntuples 2 '(1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-monad sequence-m
  (defn nth-generation
    [n cls]
    ( (m-chain (replicate n parents) cls ))))

(map #(nth-generation % #{}) (range 10))

(nth-generation 0 (class #{}))
(nth-generation 1 (class #{}))
(nth-generation 2 (class #{}))
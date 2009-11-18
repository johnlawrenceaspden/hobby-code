;;Peter Norvig's spelling corrector for clojure

(defn words [text] (re-seq #"[a-z']+" (.toLowerCase text)))
;;(words "the cat sat on the mat")

(defn train [features]
   (reduce (fn [model f] (assoc model f (inc (get model f 1)))) {} features))
;;(train (words "the cat sat on the mat"))
;;but why on earth is the default value 1?

(def *nwords* (train (words (slurp "/home/john/clojure-tutorial.txt"))))
;;(println *nwords*)

(defn edits1 [word]
   (let [alphabet "'abcdefghijklmnopqrstuvwxyz", n (count word)]
     (distinct (concat
       (for [i (range n)] (str (subs word 0 i) (subs word (inc i))))
       (for [i (range (dec n))]
         (str (subs word 0 i) (nth word (inc i)) (nth word i) (subs word (+ 2 i))))
       (for [i (range n) c alphabet] (str (subs word 0 i) c (subs word (inc i))))
       (for [i (range (inc n)) c alphabet] (str (subs word 0 i) c (subs word i)))))))

;;(edits1 "to")

(defn known [words nwords] (for [w words :when (nwords w)]  w))
;;(known ["the" "cat" "weve"] *nwords*)

(defn known-edits2 [word nwords] (for [e1 (edits1 word) e2 (edits1 e1) :when (nwords e2)]  e2))
;;(distinct (known-edits2 "chat" *nwords*))

(defn correct [word nwords]
   (let [emptynil #(if (empty? %) nil %),
	 candidates (or (emptynil (known [word] nwords))
			(emptynil (known (edits1 word) nwords))
                        (emptynil (known-edits2 word nwords))
			[word])]
     (apply max-key #(get nwords % 1) candidates)))

;;(map #(correct % *nwords*) ["the" "cat" "sat" "on" "the" "were" "thnh" "wepve" "potato"])

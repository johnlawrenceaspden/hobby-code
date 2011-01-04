;; Shannon Entropy

;; Claude Shannon tells us that a random number generator has an entropy, defined
;; using a scary looking formula.

;; What is the entropy of choose (1 2 2 3)?

(defn make-chooser [coll]
  (let [c (count coll)]
    (fn[] (nth coll (rand-int c)))))

(frequencies (let [d (make-chooser '(1 2 2 3))] (map #(%) (repeat 1000 d))))


(def p112 (make-chooser '(1 2 2 3)))
;; It is three/two. Why?

(defn encoder[x]
  (case x 1 '(0 0) 2 '(1) 3 '(0 1) :error))

(defn encode [coll]
  (mapcat encoder coll))

(defn decode[coll]
  (lazy-seq 
   (when-let [s (seq coll)]
     (cond (= (first s) 1) (cons 2 (decode (rest s)))
           (= (second s) 0) (cons 1 (decode (rest (rest s))))
           (= (second s) 1) (cons 3 (decode (rest (rest s))))))))

(def s (map #(%)(repeat 10 p112)))
s ;(1 1 3 2 2 2 1 2 1 1)
(encode s) ;(0 0 0 0 0 1 1 1 1 0 0 1 0 0 0 0)
(decode (encode s)) ;(1 1 3 2 2 2 1 2 1 1)

;; Also 1/2 log 2 + 1/4 log 4 + 1/4 log 4
;; = 1/2 1 + 1/4 2 + 1/4 2
;; = 1/2 + 1/2 + 1/2 = 3/2

(count (encode (map #(%) (repeat 1000 p112)))) ;1500 ; a bit too good to be true, but it was!


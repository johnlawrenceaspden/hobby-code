;; Chapter 22 of ITILA, question about window and stars

;; Let the whole space be divided into N bits

(def N 10)

;; And let each bit have a chance of a star in it
(def avg-no-of-stars 1)
(def chance-of-star (/ avg-no-of-stars N))

;; Then we can make random star fields like
(defn random-star-field []
  (for [i (range N)] (if (< (rand) chance-of-star) 1 0)))

(random-star-field) ; (0 1 0 1 1 0 0 0 0 0)


(defn random-star-field-with-wall [w]
  (concat (repeat w 0)
          (take (- N w) (random-star-field))))

(random-star-field-with-wall 5) ; (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1)

(def sample-space
     (for [ i (range)]
       (for [wall (range N)]
         (random-star-field-with-wall wall))))

(take 5 sample-space) ; 

(defn leftmost-star [sample]
  (count (take-while #{0} sample)))

(def leftmosts (map #(map leftmost-star %) sample-space))

(def sample-size 1000)

(use '(incanter core charts stats))
(view (histogram (take sample-size (apply concat leftmosts)) :nbins 100))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dead below here
(comment
  (defn no-of-stars [sample] (reduce + sample))

  (map #(map no-of-stars %) sample-space) ; ((0 0 2 2 2 4 4 4 3 3) (0 0 1 2 2 3 1 4 3 6) (0 0 0 3 2 3 5 2 5 4) (0 1 1 1 3 3 4 3 7 4) (0 1 1 2 0 2 2 1 4 6) (0 1 1 3 3 3 4 5 3 5) (0 1 1 2 3 2 1 2 5 6) (0 0 1 0 1 2 3 3 2 3) (0 0 2 2 3 1 4 4 2 2) (0 0 1 2 2 3 2 4 4 4) (0 1 1 1 2 2 3 3 3 3) (0 1 0 3 3 3 1 3 1 6) (0 0 1 2 1 3 4 3 2 6) (0 1 1 1 3 3 4 0 5 5) (0 0 2 1 2 4 3 4 3 6) (0 1 1 2 2 4 3 4 2 5) (0 0 2 1 3 2 3 6 5 4) (0 0 0 2 1 3 3 1 5 4) (0 0 1 2 2 1 4 4 6 6) (0 1 0 1 2 2 3 3 3 6) (0 0 1 2 1 2 3 3 6 4) (0 1 1 2 2 3 2 2 6 5) (0 0 1 1 4 2 6 3 3 4) (0 1 1 3 2 4 4 4 2 2) (0 0 1 2 2 4 1 2 4 4) (0 1 1 0 1 5 2 4 5 5) (0 0 2 1 1 3 3 5 3 3) (0 0 2 2 1 2 3 5 7 5) (0 0 2 1 3 4 4 2 5 4) (0 0 1 2 2 3 1 1 5 4) (0 0 2 0 2 2 5 5 3 7) (0 0 1 2 0 0 2 3 5 3) (0 1 0 2 3 2 3 4 4 7) (0 0 0 2 4 2 2 3 3 6) (0 1 1 1 2 2 3 1 4 4) (0 0 1 3 3 0 3 3 0 8) (0 0 0 1 2 1 2 3 4 4) (0 0 2 2 3 2 4 6 5 5) (0 1 1 2 1 4 2 2 5 4) (0 0 1 2 3 5 0 3 2 2) (0 1 1 0 1 1 0 3 2 6) (0 1 2 1 1 2 4 4 6 1) (0 0 1 0 3 3 3 4 6 5) (0 1 0 2 2 3 4 3 3 6) (0 0 2 1 2 4 6 4 3 6) (0 0 1 2 1 4 4 3 6 4) (0 1 0 3 3 2 4 4 3 7) (0 1 0 1 3 1 2 3 2 5) (0 0 1 2 1 3 4 6 5 3) (0 1 1 1 3 1 3 5 3 5) (0 1 0 3 1 4 5 3 4 5) (0 1 1 2 1 2 5 4 6 3) (0 1 1 2 1 4 3 4 4 3) (0 1 2 1 3 2 0 4 4 6) (0 0 1 2 3 4 3 3 3 6) (0 0 2 0 2 2 4 4 4 4) (0 1 2 3 2 1 2 3 4 4) (0 1 1 2 0 0 1 4 4 6) (0 1 2 1 2 4 3 3 4 4) (0 0 1 0 2 4 3 4 3 4) (0 0 1 1 1 4 4 4 4 4) (0 0 2 1 2 3 3 5 2 6) (0 0 2 2 1 3 5 5 2 3) (0 0 2 2 4 2 2 4 5 3) (0 1 2 2 3 2 5 5 2 8) (0 0 2 1 1 4 3 5 5 5) (0 0 2 2 1 5 3 3 3 5) (0 1 0 1 3 2 3 4 6 3) (0 1 2 1 2 2 2 3 4 5) (0 1 1 3 1 4 2 5 4 5) (0 0 1 3 1 3 5 4 6 5) (0 0 1 1 2 3 4 2 7 5) (0 1 0 3 2 4 1 2 2 5) (0 0 2 1 1 1 2 4 3 4) (0 0 1 0 4 3 2 4 7 4) (0 0 1 1 3 4 2 4 7 3) (0 0 1 2 2 3 3 4 5 4) (0 0 1 1 1 3 0 3 3 4) (0 0 1 1 0 3 5 3 4 4) (0 0 1 0 2 1 3 5 7 6) (0 1 2 0 2 3 2 2 1 2) (0 0 0 1 2 2 3 3 6 5) (0 0 2 2 1 3 2 3 4 6) (0 1 0 3 2 3 2 3 3 6) (0 1 0 2 1 3 1 3 6 4) (0 1 2 1 0 3 2 5 3 6) (0 0 0 2 0 2 3 5 2 6) (0 1 1 0 3 1 2 4 4 6) (0 1 2 3 3 2 3 3 4 3) (0 0 1 0 2 0 6 5 5 3) (0 0 1 2 3 5 3 2 3 3) (0 1 2 1 2 2 3 6 3 4) (0 0 2 2 3 3 3 5 2 6) (0 0 1 1 2 2 3 5 5 4) (0 0 1 2 2 4 5 2 3 3) (0 0 1 1 1 2 4 5 2 3) (0 1 1 2 2 3 5 6 3 3) (0 0 1 2 4 2 4 3 5 6) (0 0 1 0 1 2 3 3 3 8) (0 0 2 2 0 0 2 5 2 5) (0 1 1 2 2 4 2 2 3 5) (0 0 1 1 3 3 4 2 3 2) (0 0 1 3 2 1 5 3 6 3) ...)

  (defn averages [sq]
    (map (fn [[s c]] (/ s c))
         (drop 1
               (reductions (fn [[sum count] x] [(+ x sum) (inc count)]) [0.0 0] sq)))))



;; Chapter 22 of ITILA, question about window and stars

;; Let the whole space be divided into discrete bins

;; Then we can make random star fields like
(defn random-star-field [chance-of-star]
  (repeatedly #(if (< (rand) chance-of-star) 1 0)))

(random-star-field 0.1) ; (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 ...)

(defn leftmost-star [starfield]
  (count (take-while #{0} starfield)))

(leftmost-star (random-star-field 0.1)) ; 14

(defn sky-view [sky-size wall-position chance-of-star]
  {:wall-position wall-position
   :chance-of-star chance-of-star
   :star-field (concat (take (inc wall-position) (random-star-field 0))
                       (take (dec (- sky-size wall-position)) (random-star-field chance-of-star)))})

(sky-view 10 3 0.95 )
;; {:wall-position 3, :chance-of-star 0.95, :star-field (0 0 0 1 0 1 1 1 1 1)}
(sky-view 10 3 0.15 )
;; {:wall-position 3, :chance-of-star 0.15, :star-field (0 0 0 0 0 0 0 0 0 0)}

(defn make-random-sample [sky-size chance-of-star]
     "uniform distribution of wall, fixed starfield density"
     (sky-view sky-size (rand-int sky-size) chance-of-star))

(repeatedly #(make-random-sample 10 0.1))

(defn stats [sky-view]
  {:leftmost (leftmost-star (sky-view :star-field))
   :sky-size (count (sky-view :star-field))
   :wall-pos (sky-view :wall-position)})


(use '(incanter core charts stats))

(defn sample-data [sample-size sky-size avg-stars]
  (let [star-chance (/ avg-stars sky-size)]
    (take sample-size
          (map stats
               (repeatedly #(make-random-sample sky-size star-chance))))))

(let [s (sample-data 100 10 6)]
  (view (scatter-plot
         (map :leftmost s)
         (map :wall-pos s))))

(defn wall-positions-by-first-star [sky-size avg-stars star-position sample-size]
  (map :wall-pos
       (filter #(= (:leftmost %) star-position)
               (sample-data sample-size sky-size avg-stars))))

(for [pos (range 1 (inc 20))]
  (view (histogram (wall-positions-by-first-star 20 4 pos 10000) :nbins pos)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dead below here
(comment
  (defn no-of-stars [sample] (reduce + sample))

  (map #(map no-of-stars %) sample-space) 

  (defn averages [sq]
    (map (fn [[s c]] (/ s c))
         (drop 1
               (reductions (fn [[sum count] x] [(+ x sum) (inc count)]) [0.0 0] sq)))))



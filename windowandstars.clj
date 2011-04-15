;; Chapter 22 of ITILA, question about window and stars

;; Let the whole space be divided into N bits

;; Then we can make random star fields like
(defn random-star-field [chance-of-star]
  (repeatedly #(if (< (rand) chance-of-star) 1 0)))

(random-star-field 0.1) ; (0 0 0 1 0 0 1 0 0 0 0 ...)

(defn leftmost-star [starfield]
  (count (take-while #{0} starfield)))

(leftmost-star (random-star-field 0.1)) ; 14

(defn sky-view [sky-size wall-position chance-of-star]
  {:wall-position wall-position
   :chance-of-star chance-of-star
   :star-field (concat (take wall-position (random-star-field 0))
                       (take (- N wall-position) (random-star-field chance-of-star)))})

(sky-view 10 3 0.95 )
;; {:wall-position 3, :chance-of-star 0.95, :star-field (0 0 0 1 0 1 1 1 1 1)}
(sky-view 10 3 0.15 )
;; {:wall-position 3, :chance-of-star 0.15, :star-field (0 0 0 0 0 0 0 0 0 0)}

(defn make-random-sample [sky-size chance-of-star]
     "uniform distribution of wall, fixed starfield density"
     (random-star-field-with-wall sky-size (rand-int sky-size) chance-of-star))

(repeatedly #(make-random-sample 10 0.1))

(defn stats [sky-view]
  {:leftmost (leftmost-star (sky-view :star-field))
   :sky-size (count (sky-view :star-field))
   :wall-pos (sky-view :wall-position)})



(def data (repeatedly #(make-random-sample 10 0.5)))

(def s (map stats data))

(view (scatter-plot
       (map :leftmost s)
       (map :wall-pos s)))


(defn wall-by-first-star [pos]
  (map :wall-pos (filter #(= (:leftmost %) pos) s)))

(view (histogram (wall-by-first-star 5)))



(use '(incanter core charts stats))
(view (histogram  :nbins 100))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dead below here
(comment
  (defn no-of-stars [sample] (reduce + sample))

  (map #(map no-of-stars %) sample-space) 

  (defn averages [sq]
    (map (fn [[s c]] (/ s c))
         (drop 1
               (reductions (fn [[sum count] x] [(+ x sum) (inc count)]) [0.0 0] sq)))))



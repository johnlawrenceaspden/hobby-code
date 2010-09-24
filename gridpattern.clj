(use 'simple-plotter)

(defn draw-grid [step]
  (let [h (get-height) w (get-width)]
    (doseq [i (range 0 w step)]
      (plot i 0) (draw 0 h))
    (doseq [i (range 0 h step)]
      (line 0 i w i))))

(defn draw-grids [n colors]
  (when (and (<= n (get-width)) (seq colors))
    (ink (first colors))
    (draw-grid n)
    (recur (* n 2) (rest colors))))

(def g1 (create-window "grid1" 300 400))
(def g2 (create-window "grid2" 300 400))

(window g1)
(draw-grids 2 (list red yellow green))

(window g2)
(draw-grids 2 (repeat white))

(window g1)
(ink blue)
(draw-grid 16)


(use 'simple-plotter)

(defn draw-grid [step]
  (let [h (get-height) w (get-width)]
    (doseq [i (range 0 w step)] (plot i 0) (draw 0 h))
    (doseq [i (range 0 h step)] (line 0 i w i))))

(defn draw-grids [n]
  (when (>= n 5)
    (cls)
    (draw-grid n)
    (recur (/ n 2))))

(create-window)
(draw-grids 80)


;;kills original window. fix.

(create-window "grid" 300 400)
(draw-grids 256)



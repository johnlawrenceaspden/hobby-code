(defn r-squared [x y] 
 (+ (* (- 0.5 x) (- 0.5 x)) 
    (* (- 0.5 y) (- 0.5 y))))

(defn hit[]
  (let [x (rand) y (rand)]
    (< (r-squared x y) 0.25)))


(frequencies (for [i (range 1000)] (hit))) ; {true 787, false 213}
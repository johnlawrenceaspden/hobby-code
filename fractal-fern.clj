(use 'simple-plotter)

(defn transform [[xx xy yx yy dx dy]]
  (fn [[x y]] [(+ (* xx x) (* xy y) dx)
               (+ (* yx x) (* yy y) dy)]))

(def barnsleys-fern '((2  [  0    0     0     0.16 0 0    ])
                      (6  [  0.2 -0.26  0.23  0.22 0 0    ])
                      (7  [ -0.15 0.28  0.26  0.24 0 0.44 ])
                      (85 [  0.85 0.04 -0.004 0.85 0 1.6  ])))

(defn choose [lst] (let [n (count lst)] (nth lst (rand n))))

(defn iteration [transforms]
  (let [transform-list (mapcat (fn [[n t]] (repeat n (transform t))) transforms)]
    (fn [x] ((choose transform-list) x))))

(def barnsley-points (iterate (iteration barnsleys-fern) [0 1]))



(create-window "Barnsley's Fern" 350 450)

(cls)
(ink green)
(scaled-scatter-plot (take 10000 barnsley-points) 50 300 50 400 100)





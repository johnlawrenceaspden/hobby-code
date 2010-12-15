(use 'simple-plotter)

(def tinyrange '(1
                 1/10000000000 
                 1/100000000000000000000000 
                 1/100000000000000000000000000000000000000000000000
                 0.1 
                 0.00000000000000000000000001
                 0.000000000000000000000000000000000000000000000001))



(for [tiny tinyrange]
  (let [xcoords (range (- tiny) tiny (/ tiny 100))
        ycoords (map #(* tiny (Math/sin (* Math/PI (/ % tiny)))) xcoords)
        pairs (partition 2 (interleave xcoords ycoords))]

    (create-window (str "minute scales:" tiny) 400 300 white black  (- tiny) tiny (- tiny) tiny)
    (axes)
    (doseq [[x y] pairs] (draw-to x y))))
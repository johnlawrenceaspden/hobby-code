;; Numerical solution of dy/dt = f (t,y)


(defn f [t y] (- t y))

(defn iterator [[t0 y0 h]]
  (let [t1 (+ t0 h)
        y1 (+ y0 (* h (f t0 y0)))]
    [t1 y1 h]))


(def iterations (iterate iterator [0.0 1.0 0.01]))

(time (count (take 1000000 iterations)))
"Elapsed time: 10874.675577 msecs"

;; Numerical solution of dy/dt = f (t,y)


(defn f [t y] (- t y))

(defn iterator [[t0 y0 h]]
  (let [t1 (+ t0 h)
        y1 (+ y0 (* h (f t0 y0)))]
    [t1 y1 h]))


(def iterations (iterate iterator [0.0 1.0 0.01]))

(time (nth iterations 100000))
"Elapsed time: 545.998816 msecs"

;; 1 600 000 000 ops/sec
;; 1 600 000 ops/msec
;; 545 msecs/ 100 000 its = 0.00545 msecs/it (* 0.00545 1600000)
;; 8720 ops/it??

;; ops/it=ghz*nanotime/its

(defmacro opsperit [expr ghz, its]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         finish# (. System (nanoTime))]
     [(/ (* ~ghz (- finish# start#)) ~its), ret#]))

(opsperit (nth iterations 100000) 1.6 100000)
[2880.876992 [999.9999999992356 998.9999999992357 0.01]]
[2918.492992 [999.9999999992356 998.9999999992357 0.01]]

(defn solveit [t0 y0 h its]
  (if (> its 0) 
    (let [t1 (+ t0 h)
          y1 (+ y0 (* h (f t0 y0)))]
      (recur t1 y1 h (dec its)))
    [t0 y0 h its]))

(opsperit (solveit 0.0 1.0 0.01 1000000) 1.6 1000000)
[1867.9925168000002 [10000.000000171856 9999.000000171847 0.01 0]]


(defn solveit [t0 y0 h its]
  (loop [t0 t0 y0 y0 h h its its]
    (if (> its 0) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (f t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

(opsperit (solveit 0.0 1.0 0.01 1000000) 1.6 1000000)
[1687.9692032 [10000.000000171856 9999.000000171847 0.01 0]]

(set! *warn-on-reflection* true)

(defn solveit [t0 y0 h its]
  (loop [ t0 (float t0)
         y0 (float y0)
         h (float h)
         its (int its)]
    (if (> its 0) 
      (let [t1 (float (unchecked-add t0 h))
            y1 (float (unchecked-add y0 (unchecked-multiply h (float (f t0 y0)))))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

(opsperit (solveit 0.0  1.0 0.01 1000000) 1.6 1000000)
[1483.4707072 [9865.224 9864.235 0.01 0]]







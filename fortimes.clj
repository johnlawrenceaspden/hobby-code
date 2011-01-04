(defmacro fortimes [[i end] & code]
  `(let [finish# ~end]
     (loop [~i 0 results# '()]
       (if (< ~i finish#)
         (recur (inc ~i) (cons (do ~@code) results#))
         (reverse results#)))))

(fortimes [x 10]
          (print x)
          (* x x))

(macroexpand '(fortimes [x 10] (* x x)))


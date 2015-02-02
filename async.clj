(use 'clojure.core.async)

(defn foo [n]
  (let [c1 (chan)
        c2 (chan)]
    (async/go-loop [n (dec n)]
      (>! c1 "Fizz")
      (<! (timeout (rand 500)))
      (when (pos? n) (recur (dec n))))
    (async/go-loop [n (dec n)]
      (>! c2 41)
      (<! (timeout (rand 500)))
      (when (pos? n) (recur (dec n))))
    (async/go-loop [n (dec (* 2 n))]
      (let [[v c] (async/alts! [c1 c2])]
        (condp = c
          c1 (println (str v "Buzz"))
          c2 (println (inc v))))
      (when (pos? n) (recur (dec n))))))


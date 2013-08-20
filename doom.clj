;; to see the benefits of slow doom, you'd need to get nrepl-ritz type
;; debugging to work reliably


(defn doom[]
  (/ 1 0))

(defn slowdoom[n]
  (if (zero? n) (doom) (recur (dec n))))

(slowdoom 20)

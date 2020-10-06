(defn factorial [n]
  (if (< n 2) n (* n (factorial (dec n)))))

(factorial 6)


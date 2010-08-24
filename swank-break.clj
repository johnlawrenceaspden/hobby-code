;;Run this under emacs to make swank-clojure display locals

(defn factorial [n]
  (when (= n 23) (swank.core/break))
        (if (< n 2) n
            (* n (factorial (dec n)))))

(factorial 30)
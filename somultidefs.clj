(defmacro multidef[n]
  `(do ~@(for [i (range n)]
           `(def ~(symbol (str "i" i)) ~i))))

(multidef 128)

i0   ; 0
i127 ;127
i128 ; unable to resolve
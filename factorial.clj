(defn factorial [n]
        (if (= n 0) 
          1 
          (* n (factorial (dec n)))))

(defmacro dotest [str & tests]
  `(if (and ~@tests) 
     (print ~str "passed") 
     (print ~str "failed")))

(dotest "factorial"
     (= (factorial 1) 1)
     (= (factorial 2) 2)
     (= (map factorial (range 10)) '(1 1 2 6 24 120 720 5040 40320 362880)))
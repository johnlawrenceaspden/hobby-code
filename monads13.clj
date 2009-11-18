(use 'clojure.contrib.monads)

(domonad sequence-m
         [x (range 5)
          y (range 5) :when (< x y)
          z (range 5) :when (< y z)]
         (list x y z))

(with-monad sequence-m
  (domonad
   [x '(1 2 3)
   y '(\a \b \c)]
   [x y]))

(domonad sequence-m
         [x (range 10)
          y (range (+ x 1)) :when (=(+ x y) 7)]
          (list x y))

(with-monad sequence-m
  (defn pairs [xs]
    ((m-lift 2 #(list %1 %2)) xs xs)))

(pairs (range 5))


(with-monad sequence-m
  (defn pairs [xs]
    (m-seq (list xs xs))))

(macroexpand-r
'(with-monad sequence-m
  (defn n-tuples [n xs]
    (m-seq (replicate n xs)))))

(pairs (range 5))
(n-tuples 3 (range 2))
(n-tuples 4 (range 2))

(defn mapr
  "recursively maps a function to a sequence and
  subsequences within the map results."
  [f & sequence]
  (defn maprec [form]
    (if (seq? form)
      (map maprec (f form))
      form))
  (first (maprec sequence))) 


(defn macroexpand-r
  "Expands all macros in a form and its subforms."
  [forms]
  (mapr macroexpand forms)) 



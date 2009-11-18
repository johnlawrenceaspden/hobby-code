(use 'clojure.contrib.monads)

(for [a (range 5)
      :when (odd? a)]
  (* a a))

(domonad sequence-m
         [a (range 5)
          :when (odd? a)]
         (* a a))
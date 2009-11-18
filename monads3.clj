(use 'clojure.contrib.monads)

(domonad identity-m
         [a 3
          b 4
          a2 (* a a)
          b2 (* b b)
          c2 (+ a2 b2)]
         (Math/sqrt c2))

(domonad maybe-m
         [a 3
          b 4
          a2 (* a a)
          b2 (* b b)
          c2 (+ a2 b2)]
         (Math/sqrt c2))
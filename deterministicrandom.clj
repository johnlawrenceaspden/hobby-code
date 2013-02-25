;; There should be 64 bits in a random long. Each random number in 0..7 is three bits

;; So we can get 21 random numbers in 0..7 out of a random long, with a bit left
(def doom (java.util.Random. 0)) ;-> #'user/doom
(unpack (repeat 21 8) (.nextLong doom)) ;-> (0 7 4 0 5 2 6 6 4 2 7 7 5 0 5 5 0 4 4 5 3)
;; Attempts to get more are somewhat unsuccessful
(unpack (repeat 22 8) (.nextLong doom)) ;-> (6 7 2 0 7 2 6 1 3 3 6 4 7 6 2 6 3 2 6 6 3 0)
(unpack (repeat 23 8) (.nextLong doom)) ;-> (2 7 0 0 7 3 7 0 7 1 1 3 7 3 2 6 5 5 4 1 2 7 7)
(unpack (repeat 23 8) (.nextLong doom)) ;-> (3 0 4 2 2 0 5 7 5 3 4 3 3 1 4 3 1 5 3 6 0 7 7)
(unpack (repeat 23 8) (.nextLong doom)) ;-> (5 5 5 3 2 5 4 0 0 1 7 6 3 4 6 5 0 7 3 4 1 7 7)
(unpack (repeat 23 8) (.nextLong doom)) ;-> (3 1 1 0 7 3 5 5 0 0 5 5 2 2 3 6 5 1 5 2 5 0 0)
(unpack (repeat 23 8) (.nextLong doom)) ;-> (5 1 5 4 6 7 3 7 4 3 2 1 2 4 0 6 3 3 2 1 6 0 0)
(unpack (repeat 23 8) (.nextLong doom)) ;-> (0 3 5 7 0 4 6 4 3 7 3 7 2 6 4 4 6 3 0 6 7 7 7)
(unpack (repeat 23 8) (.nextLong doom)) ;-> (7 0 4 0 3 1 2 7 5 0 0 4 0 6 6 0 2 2 4 0 6 7 7)
(unpack (repeat 23 8) (.nextLong doom)) ;-> (1 6 3 0 5 1 3 0 5 5 0 1 7 1 5 5 5 6 3 0 7 7 7)


;; Linear Congruential Random Number Generators

(defn iterator [a b]
  (fn[x] (mod (+ (* a x) b) (bit-shift-left 1 31))))

(def bsd (drop 1 (iterate (iterator 1103515245 12345) 0)))

(def ms (drop 1 (for [x (iterate  (iterator 214013 2531011) 0)] (bit-shift-right x 16))))

(take 10 bsd) ;-> (12345 1406932606 654583775 1449466924 229283573 1109335178 1051550459 1293799192 794471793 551188310)
(take 10 ms) ;-> (38 7719 21238 2437 8855 11797 8365 32285 10450 30612)

(time (nth (drop 1 (iterate (iterator 1103515245 12345) 0)) 1000000))
"Elapsed time: 2587.596789 msecs"
1905486841

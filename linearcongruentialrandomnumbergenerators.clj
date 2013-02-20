;; Linear Congruential Random Number Generators

;; I was wondering how pseudo-random numbers are actually generated.

;; Apparently it's still done as I remember it done on the ZX80, using multiply, add and chop.

;; There was a nice article on Rosetta Code :
;; http://rosettacode.org/wiki/Linear_congruential_generator

;; But it didn't have a clojure example, so I made an account and added one:

;; This code took me a couple of minutes to write, and replicates a couple of classic generators

(defn iterator [a b c]
  (fn[x] (mod (+ (* a x) b) (bit-shift-left 1 c))))

(def bsd (drop 1 (iterate (iterator 1103515245 12345 31) 0)))

(def ms (drop 1 (for [x (iterate  (iterator 214013 2531011 31) 0)] (bit-shift-right x 16))))

(take 10 bsd) ;-> (12345 1406932606 654583775 1449466924 229283573 1109335178 1051550459 1293799192 794471793 551188310)
(take 10 ms) ;-> (38 7719 21238 2437 8855 11797 8365 32285 10450 30612)

;; What's really nice is that the natural expression of this idea in
;; Clojure is (I think) one of the neatest pieces of code on there.

;; Java uses a similar generator, but it takes some proving since it won't let you see the internal state directly:

(defn java [seed] 
  (drop 1 (iterate (iterator 0x5DEECE66D 0xB 48) seed )))

(def java-random-ints (map #(unchecked-int (bit-shift-right (long %) 16)) (java 0x5DEECE66DN)))

(take 10 java-random-ints) ;-> (-1155484576 -723955400 1033096058 -1690734402 -1557280266 1327362106 -1930858313 502539523 -1728529858 -938301587)

;; We can also get them direct from java:

(def java-util-random
  (let [r (java.util.Random. 0)]
    (for [i (range)] (.nextInt r))))

(take 10 java-util-random) ;-> (-1155484576 -723955400 1033096058 -1690734402 -1557280266 1327362106 -1930858313 502539523 -1728529858 -938301587)


;; And compare a few:

(drop-while zero? (take 100000 (map - java-util-random java-random-ints)))  ;-> ()


;; Incidentally while messing around here I discovered how to call a protected method of a java class from clojure:

(.nextInt (java.util.Random. 0) ) ;-> -1155484576
(.next (java.util.Random. 0) 32)  ; java.lang.IllegalArgumentException: No matching method found: next for class java.util.Random

;; It's lying.
;; But I know it's in there somewhere:

(defn myrandom [seed] (proxy [java.util.Random][seed]
                    (next [a] (proxy-super next a))))

(def r (myrandom 0)) ;-> #'user/r
(.nextInt (myrandom 0)) ;-> -1155484576
(.next (myrandom 0) 32) ;-> -1155484576

;; Stupid language.


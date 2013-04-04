;; I keep finding that I need a function which will partition a sequence into runs of things.

;; For instance, you might want

'(1 1 1 2 3 3 4 4 4 3 3 3 5 5)

;; to go to:

'((1 1 1) (2) (3 3) (4 4 4) (3 3 3) (5 5))

;; Which is what partition-by does:

(partition-by identity '(1 1 1 2 3 3 4 4 4 3 3 3 5 5)) ;-> ((1 1 1) (2) (3 3) (4 4 4) (3 3 3) (5 5))

;; But partition-by isn't quite what I want.

;; I'd like to be able to turn

'( 1 2 3 4 1 2 3 2 3 4 6 9 10)

;; Into

'((1 2 3 4) (1 2 3) (2 3 4) (6) (9 10))

;; By defining a comparator like:

(defn ^:dynamic sameish [a b] ( = (inc a) b))

;; And then saying:
;; (partition-by-equivalence sameish '( 1 2 3 4 1 2 3 2 3 4 6 9 10))

;; Doing this by hand:

;; () () '( 1 2 3 4 1 2 3 2 3 4 6 9 10)
;; ->
;; () (1) (2 3 4 1 2 3 2 3 4 6 9 10)
;; ->
;; () (1 2) (3 4 1 2 3 2 3 4 6 9 10)
;; ->
;; () (1 2 3) (4 1 2 3 2 3 4 6 9 10)
;; ->
;; () (1 2 3 4) (1 2 3 2 3 4 6 9 10)
;; -> test is false, start a new list
;; ((1 2 3 4)) (1) (2 3 2 3 4 6 9 10)
;; ->
;; ((1 2 3 4)) (1 2) (3 2 3 4 6 9 10)
;; ->
;; ((1 2 3 4)) (1 2 3) (2 3 4 6 9 10)
;; -> test is false, start a new list
;; ((1 2 3 4) (1 2 3))  (2 3 4 6 9 10)
;; ->
;; ((1 2 3 4) (1 2 3) (2))  (3 4 6 9 10)
;; -> etc

;; makes me think that this looks like a tail recursion with two accumulators

(defn ^:dynamic recaccacc [ f acc1 acc2 coll]
  (if (empty? coll) (cons acc2 acc1)
      (if (empty? acc2) (recaccacc f acc1 (cons (first coll) acc2) (rest coll))
          (if (f (first acc2) (first coll))
            (recaccacc f acc1 (cons (first coll) acc2) (rest coll))
            (recaccacc f (cons acc2 acc1) '() coll)))))



;; Unfortunately, this comes out backwards
(use 'clojure.tools.trace)

(dotrace [recaccacc] (recaccacc  sameish '() '() '(1 2 3 4 1 2 3 2 3 4 6 9 10)))

;; TRACE t1169: (recaccacc #<user$sameish user$sameish@11b99c4> () () (1 2 3 4 1 2 3 2 3 4 6 9 10))
;; TRACE t1170: | (recaccacc #<user$sameish user$sameish@11b99c4> () (1) (2 3 4 1 2 3 2 3 4 6 9 10))
;; TRACE t1171: | | (recaccacc #<user$sameish user$sameish@11b99c4> () (2 1) (3 4 1 2 3 2 3 4 6 9 10))
;; TRACE t1172: | | | (recaccacc #<user$sameish user$sameish@11b99c4> () (3 2 1) (4 1 2 3 2 3 4 6 9 10))
;; TRACE t1173: | | | | (recaccacc #<user$sameish user$sameish@11b99c4> () (4 3 2 1) (1 2 3 2 3 4 6 9 10))
;; TRACE t1174: | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((4 3 2 1)) () (1 2 3 2 3 4 6 9 10))
;; TRACE t1175: | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((4 3 2 1)) (1) (2 3 2 3 4 6 9 10))
;; TRACE t1176: | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((4 3 2 1)) (2 1) (3 2 3 4 6 9 10))
;; TRACE t1177: | | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((4 3 2 1)) (3 2 1) (2 3 4 6 9 10))
;; TRACE t1178: | | | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((3 2 1) (4 3 2 1)) () (2 3 4 6 9 10))
;; TRACE t1179: | | | | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((3 2 1) (4 3 2 1)) (2) (3 4 6 9 10))
;; TRACE t1180: | | | | | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((3 2 1) (4 3 2 1)) (3 2) (4 6 9 10))
;; TRACE t1181: | | | | | | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((3 2 1) (4 3 2 1)) (4 3 2) (6 9 10))
;; TRACE t1182: | | | | | | | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((4 3 2) (3 2 1) (4 3 2 1)) () (6 9 10))
;; TRACE t1183: | | | | | | | | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((4 3 2) (3 2 1) (4 3 2 1)) (6) (9 10))
;; TRACE t1184: | | | | | | | | | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((6) (4 3 2) (3 2 1) (4 3 2 1)) () (9 10))
;; TRACE t1185: | | | | | | | | | | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((6) (4 3 2) (3 2 1) (4 3 2 1)) (9) (10))
;; TRACE t1186: | | | | | | | | | | | | | | | | | (recaccacc #<user$sameish user$sameish@11b99c4> ((6) (4 3 2) (3 2 1) (4 3 2 1)) (10 9) ())
;; TRACE t1186: | | | | | | | | | | | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1185: | | | | | | | | | | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1184: | | | | | | | | | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1183: | | | | | | | | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1182: | | | | | | | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1181: | | | | | | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1180: | | | | | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1179: | | | | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1178: | | | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1177: | | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1176: | | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1175: | | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1174: | | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1173: | | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1172: | | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1171: | | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1170: | => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))
;; TRACE t1169: => ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))



;-> ((10 9) (6) (4 3 2) (3 2 1) (4 3 2 1))

;; Which we can fix:

(reverse (map reverse (recaccacc sameish '() '() '(1 2 3 4 1 2 3 2 3 4 6 9 10))))
;-> ((1 2 3 4) (1 2 3) (2 3 4) (6) (9 10))

;; Hooray!

;; So our first definition is

(defn partition-by-equivalence [f coll]
  (let [recaccacc (fn [f acc1 acc2 coll]
                    (if (empty? coll) (reverse (cons (reverse acc2) acc1))
                        (if (empty? acc2) (recur f acc1 (cons (first coll) acc2) (rest coll))
                            (if (f (first acc2) (first coll))
                              (recur f acc1 (cons (first coll) acc2) (rest coll))
                              (recur f (cons (reverse acc2) acc1) '() coll)))))]
    (recaccacc f '() '() coll)))



(partition-by-equivalence sameish '(1 2 3 4 1 2 3 2 3 4 6 9 10)) ;-> ((1 2 3 4) (1 2 3) (2 3 4) (6) (9 10))

(partition-by-equivalence sameish '()) ;-> (())
(partition-by-equivalence sameish '(1)) ;-> ((1))
(partition-by-equivalence sameish '(1 1)) ;-> ((1) (1))
(partition-by-equivalence sameish '(1 2)) ;-> ((1 2))
(partition-by-equivalence sameish '(1 2 1)) ;-> ((1 2) (1))
(partition-by-equivalence sameish '(1 2 1 1)) ;-> ((1 2) (1) (1))
(partition-by-equivalence sameish '(1 2 1 1 2 2)) ;-> ((1 2) (1) (1 2) (2))

;; Here's some incomprehensible maths-stuff about numbers of digits and logarithms and so on.
(map count (partition-by (fn[a] (int (Math/log a))) (range 1 10000))) ; (2 5 13 34 94 255 693 1884 5123 1896)
(partition-by-equivalence (fn [a b] (= (int (Math/log a)) (int (Math/log b)))) (range 1 100)) ;-> ((1 2) (3 4 5 6 7) (8 9 10 11 12 13 14 15 16 17 18 19 20) (21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54) (55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99))
(partition-by-equivalence (fn [a b] (= (int (Math/log10 a)) (int (Math/log10 b)))) (range 1 100)) ;-> ((1 2 3 4 5 6 7 8 9) (10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99))

;; ascending subsequences
(partition-by-equivalence <= '(1 2 3 3 4 5 7 8 9 1 2 5 6 1 7 8))
;-> ((1 2 3 3 4 5 7 8 9) (1 2 5 6) (1 7 8))

;; strictly ascending subsequences
(partition-by-equivalence < '(1 2 3 3 4 5 7 8 9 1 2 5 6 1 7 8))
;-> ((1 2 3) (3 4 5 7 8 9) (1 2 5 6) (1 7 8))


;; lengths of increasing runs
(map count (partition-by-equivalence <= '(1 2 3 3 4 5 7 8 9 1 2 5 6 1 7 8)) ) ;-> (9 4 3)
;; lengths of decreasing ones
(map count (partition-by-equivalence >= '(1 2 3 3 4 5 7 8 9 1 2 5 6 1 7 8)) ) ;-> (1 1 2 1 1 1 1 2 1 1 2 1 1)

;; and finally, a simplified version of the latest problem I actually needed this for, pulling a sequence of lists of scores out of a log file
;; so that each full score list only appears once, and all its ancestors are discarded.
(map last (partition-by-equivalence (fn[a b] (= a (drop 1 b))) '( () (1) (2 1) (3 2 1) () (9) (7 9)))) ;-> ((3 2 1) (7 9))




;; It's a strict generalization of partition-by
(defn my-partition-by [f coll]
  (partition-by-equivalence (fn[a b] (= (f a) (f b))) coll))

(map #(/ (Math/log %) (Math/log 2)) (range 1 100))
(my-partition-by #(int(/ (Math/log %) (Math/log 2))) (range 1 100))


;; And I think it's a really nice function, which is helpful in all sort of situations.

;; It should be possible to make it completely lazy, so that it can take infinite inputs without wolfing the lot.

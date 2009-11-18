; SLIME 2009-08-21
user> (+ 1 2)
3
user> (reduce (fn [m k](assoc m k (inc (get m k 0)))) {} [1 2 3 4 5 6 7 5 3 2])
{7 1, 6 1, 5 2, 4 1, 3 2, 2 2, 1 1}
user> (re-seq #"\w+" "the cat sat on the mat")
("the" "cat" "sat" "on" "the" "mat")
user> (reduce (fn [m k](assoc m k (inc (get m k 0)))) {}(re-seq #"\w+" "the cat sat on the mat") )
{"mat" 1, "on" 1, "sat" 1, "cat" 1, "the" 2}
user> (defn word-count [s] (reduce (fn [m k](assoc m k (inc (get m k 0)))) {}(re-seq #"\w+" s) ))
#'user/word-count
user> (word-count "the cat sat on the mat")
{"mat" 1, "on" 1, "sat" 1, "cat" 1, "the" 2}
user> (get {} "hello")
nil
user> (get {"hello" 1, "hi" 3} "hi")
3
user> (get {"hello" 1, "hi" 3} "howdy")
nil
user> (get {"hello" 1, "hi" 3} "howdy" 0)
0
user> (get {"hello" 1, "hi" 3} "hi" 0)
3
user> (assoc {} "hi" (inc (get {} "hi" 0)))
{"hi" 1}
user> (fn [map word] (assoc map word (inc (get map word 0))))
#<user$eval__3469$fn__3471 user$eval__3469$fn__3471@a224b5>
user> ((fn [map word] (assoc map word (inc (get map word 0)))) {} "hi")
{"hi" 1}
user> ((fn [map word] (assoc map word (inc (get map word 0)))) {"hi" 1} "hi")
{"hi" 2}
user> ((fn [map word] (assoc map word (inc (get map word 0)))) {"hi" 2} "hello")
{"hello" 1, "hi" 2}
user> (defn add-word [map word] (assoc map word (inc (get map word 0))))
#'user/add-word
user> (add-word {} "hi")
{"hi" 1}
user> (add-word {"hi" 1} "hi")
{"hi" 2}
user> (add-word {"hi" 2} "hello")
{"hello" 1, "hi" 2}
user> (add-word {"hi" 2} "hello")
{"hello" 1, "hi" 2}
user> (add-word (add-word {"hi" 2} "hello") "yo")
{"yo" 1, "hello" 1, "hi" 2}
user> (reduce add-word  {} ["the" "cat" "sat" "on" "the" "map"])
{"map" 1, "on" 1, "sat" 1, "cat" 1, "the" 2}
user> (add-word "map" (add-word "the" (add-word "on" (add-word "sat" (add-word "cat" (add-word "the" {}))))))
; Evaluation aborted.
user> (add-word {} "the")
{"the" 1}
user> (add-word (add-word (add-word {} "the") "cat") "sat")
{"sat" 1, "cat" 1, "the" 1}
user> (reduce add-word {} ["the" "cat" "sat"])
{"sat" 1, "cat" 1, "the" 1}
user> (reduce add-word {} ["the" "cat" "sat" "on" "the" "map"])
{"map" 1, "on" 1, "sat" 1, "cat" 1, "the" 2}
user> (re-seq "\w+" "the cat sat on the map")
; Evaluation aborted.
user> (re-seq #"\w+" "the cat sat on the map")
("the" "cat" "sat" "on" "the" "map")
user> (reduce add-word {} (re-seq #"\w+" "the cat sat on the map"))
{"map" 1, "on" 1, "sat" 1, "cat" 1, "the" 2}
user> xs
; Evaluation aborted.
user> 
;; Done it again!

;; Have you ever wanted to filter a sequence?

(filter odd? '(1 2 3 4 5))  ;-> (1 3 5)

;; But as well as keeping the bits that satisfy the predicate, you want the bits that don't as well?

(filter (complement odd?) '(1 2 3 4 5)) ; -> (2 4)

;; I do this so often that I wrote a little function to do it for me:


(defn split-filter [predicate sequence]
  "split a sequence into two sequences by predicate"
  [(filter predicate sequence) (filter (complement predicate) sequence)])


(split-filter odd? '(1 2 3 4 5)) ; ->  [(1 3 5) (2 4)]

;; Handy huh?, but soon after I wanted to split a sequence three ways

(filter #(= 1 (count %)) '((1) (1 2) (3 4) (3 4 5) (9) (8) (7 6))) ; -> ((1) (9) (8))

(filter #(= 2 (count %)) '((1) (1 2) (3 4) (3 4 5) (9) (8) (7 6))) ; -> ((1 2) (3 4) (7 6))

(filter #(= 3 (count %)) '((1) (1 2) (3 4) (3 4 5) (9) (8) (7 6))) ; -> ((3 4 5))

;; And of course that leads to a new function, to split a sequence by an arbitrary function

(defn hash-filter [pred coll]
    "splits sequence by predicate into a hash of predicate results to (reversed) subsequence"
    (reduce (fn [m x] (assoc m (pred x) (cons x (get m (pred x) '())))) {} coll))

(hash-filter count '((1) (1 2) (3 4) (3 4 5) (9) (8) (7 6))) ; ->
                                        ;{  3 ((3 4 5)),
                                        ;   2 ((7 6) (3 4) (1 2)),
                                        ;   1 ((8) (9) (1)) }


;; And so at this point I am feeling nice and smug, because I have invented a nice new abstraction and
;; evidently it is very useful (in fact I used it several times in my program).

;; And I am wary of that feeling, because I have had it before.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clojure.core is very good. It is beautifully documented on a single page here:

;; http://clojure.org/cheatsheet

;; There is even a nicely coloured A4 version on a pdf, which fits on one double sided sheet.

;; I have now printed out that single A4 page, and have it next to my computer.

;; Looking at it has led me to:

(defn group-by 
  "Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  {:added "1.2"}
  [f coll]  
  (persistent!
   (reduce
    (fn [ret x]
      (let [k (f x)]
        (assoc! ret k (conj (get ret k []) x))))
    (transient {}) coll)))

;; Note the nice use of persistent and transient together with vector conj so that you don't need to reverse the sequences
;; if you want them to stay in the same order.

;; Note the fact that group-by only evaluates the predicate once for each member.

;; I hadn't even noticed that I'd typed that twice.
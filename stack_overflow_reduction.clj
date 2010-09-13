;; How to turn:
[[:a 1 2] [:a 3 4] [:a 5 6] [:b \a \b] [:b \c \d] [:b \e \f]]
;; into:
{:a [[1 2] [3 4] [5 6]] :b [[\a \b] [\c \d] [\e \f]]}
;; ?

(def input [[:a 1 2] [:a 3 4] [:a 5 6] [:b \a \b] [:b \c \d] [:b \e \f]])

;; my answer, but someone had already posted it. boo.
(reduce
 (fn [m [k & stuff]]
     (assoc m k (conj (m k []) (vec stuff))))
 {}
 input)

;; this is even awesomer
(let [yo (group-by first input)]
  (zipmap (keys yo) (map #(vec (map (comp vec rest) %)) (vals yo))))
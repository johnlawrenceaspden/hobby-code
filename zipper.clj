;; What is a zipper for?
(use 'clojure.zip)

;; Short answer: editing trees
;; suppose we want to increment caddar and cadddar, and replace caddddar
;; e.g.
;((x x (x     9  10)) x (x x (x x)))
;((x x (hello 10 11)) x (x x (x x)))


;;Then we can, thinking about a moving cursor and imperative replacement:
(-> (seq-zip '((1 2 (3 9 10)) 4 (5 6 (7 8)))) 
    down 
    down 
    right 
    right 
    down 
    (replace 'hello)
    right 
    (edit inc) 
    right
    (edit inc)
    root)

;; Long answer

;; Consider trying to modify a list, say (1 2 3 4)
;; We'd like to increment the third element

(cons 1 (cons 2 (cons (inc 3) (cons 4 nil))))

;; following the convention that the third element is element 2 (sigh)
;; we could define the following function
(defn modify-nth [n fn lst]
  (if (empty? lst) '()
      (if (= n 0) (cons (fn (first lst)) (rest lst))
          (cons (first lst) (modify-nth (dec n) fn (rest lst))))))

(modify-nth 2 inc '(1 2 3 4))

;; If it's a tree we'd want to modify, then we could imagine a function
;; which 

;;nice example of reduce


(reduce (fn [a b] (assoc a b (inc (a b 0)))) {} "fluffy!")





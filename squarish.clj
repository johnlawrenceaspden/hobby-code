;; Microsoft Research 15th Anniversary Problem: A special squarish age.

;; A number is squarish if it is the product of two consecutive integers.
;; e.g. 6 is squarish because it is the product of 2 and 3

;; A colleague claims that his age is squarish, and furthermore, that his last squarish birthday was a squarish number of years ago. What age could he be?

(def squarish (map (fn[[x y]] (* x y)) (partition 2 1 (iterate inc 0))))
(def squarishdiffs (map - (drop 1 squarish) squarish))

(defn find-in-increasing-seq [x seq]
  (cond (empty? seq) false
        (< (first seq) x) (recur x (rest seq))
        (= (first seq) x) true
        (> (first seq) x) false))

(defn squarish? [x] (find-in-increasing-seq x squarish))

(def possible-ages (mapcat 
                    (fn [s d] (if (squarish? d) (list s) '())) 
                    (drop 1 squarish) 
                    squarishdiffs))

(take 10 possible-ages)

;; He's probably 42. 12 is a bit young for a colleague, 110 is pushing it.
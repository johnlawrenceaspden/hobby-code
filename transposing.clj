;; Transposition

;; In life, a man often finds himself transposing matrices

;; Here is a matrix A
;; [ 1  2  3  4 ]
;; [ 5  6  7  8 ]
;; [ 9 10 11 12 ]

;; And here is its transpose 
;; [ 1 5  9 ]
;; [ 2 6 10 ]
;; [ 3 7 11 ]
;; [ 4 8 12 ]

;; One fairly standard way to represent a matrix is as a vector of vectors

(def A [[ 1  2  3  4 ]
        [ 5  6  7  8 ]
        [ 9 10 11 12 ]])


;; If the rows were separate, adding them would be easy, that's what map does
(map +
     [ 1  2  3  4 ]
     [ 5  6  7  8 ]
     [ 9 10 11 12 ]) ; (15 18 21 24)


;; the vector operator works similarly to +
(+ 1 2 3) ; 6
(vector 1 2 3) ; [1 2 3]


(map vector
     [ 1  2  3  4 ]
     [ 5  6  7  8 ]
     [ 9 10 11 12 ]) ; ([1 5 9] [2 6 10] [3 7 11] [4 8 12])


;; but if you have a list of things, and you want to apply a multi-argument function, you can do it with apply
(apply map + A) ; (15 18 21 24)
(apply map vector A) ; ([1 5 9] [2 6 10] [3 7 11] [4 8 12])

;; And this gives us our first version of transpose
(defn transpose [vv] (apply mapv vector vv))

;; It works well on its intended case
(transpose A) ; [[1 5 9] [2 6 10] [3 7 11] [4 8 12]] 
(transpose (transpose A)) ; [[1 2 3 4] [5 6 7 8] [9 10 11 12]]


;; deals nicely with non-vector seqs
(transpose (list [1 2 3 4](list 1 2 3 4)[1 2 3 4])) ; [[1 1 1] [2 2 2] [3 3 3] [4 4 4]]

;; and does something sensible with ragged lists
(iterate transpose (list [1 2 3 4](list 1 2 3 )[1 2])) ; (([1 2 3 4] (1 2 3) [1 2]) [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] [[1 2] [1 2] [1 2]] [[1 1 1] [2 2 2]] ...)

;; but it doesn't deal well with empty inputs
(transpose [[]]) ; []
(transpose []) ; explosion





(def m {:a [1 2 3] :b [4 5 6] :c [7 8 9]}) ; #'user/m
(into {} (for [[k v] m] [k (first v)])) ; {:a 1, :b 4, :c 7}
(into {} (for [[k v] m] [k (rest v)])) ; {:a (2 3), :b (5 6), :c (8 9)}

(defn mapmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(mapmap first m) ; {:a 1, :b 4, :c 7}
(mapmap rest m) ; {:a (2 3), :b (5 6), :c (8 9)}
(mapmap rest (mapmap rest m)) ; {:a (3), :b (6), :c (9)}
(mapmap rest (mapmap rest (mapmap rest m))) ; {:a (), :b (), :c ()}
(mapmap rest (mapmap rest (mapmap rest (mapmap rest m)))) ; {:a (), :b (), :c ()}

(defn mapempty? [m]
  (some #{true} (for [[k v] m] (empty? v))))

(mapempty? m) ; nil
(map mapempty? (iterate (partial mapmap rest) m)) ; (nil nil nil true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true true ...)

(defn map-transpose [m]
  (if (empty? m) []
      (if (mapempty? m) []
          (concat [(mapmap first m)]
                  (map-transpose (mapmap rest m))))))


;;->
(map-transpose {}) ; [] ; [] ; [] 
(map-transpose {:a [1] :b [2] :c[3]}) ; ({:a 1, :b 2, :c 3}) ;  ; [{:a 1, :b 2, :c 3}]

(map-transpose {:a [1 2] :b [2 4] :c[3 6]}) ; ({:a 1, :b 2, :c 3} {:a 2, :b 4, :c 6}) ; [{:a 1, :b 2, :c 3}]
(concat [{:a 1, :b 2, :c 3}] [{:a 2, :b 4, :c 6}]) ; ({:a 1, :b 2, :c 3} {:a 2, :b 4, :c 6}) ; ({:a 1, :b 2, :c 3} {:a 2, :b 4, :c 6})

(map-transpose {:a [1 2] :b [2 4] :c[3 6]}) ; ({:a 1, :b 2, :c 3} {:a 2, :b 4, :c 6})

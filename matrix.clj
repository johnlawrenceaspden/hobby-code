(use 'clojure.core.matrix)

(def a (matrix [[2 0] [0 2]]))
(def b (matrix [[2 0] [0 2]]))

(mul a b) ;-> [[4 0] [0 4]]

(mmul a b) ;-> [[4 0] [0 4]]

(pm (matrix [[0 1][-1 0]]))

(mmul (matrix [1 2 3]) (matrix [1 2 3]))  

(transpose a) ;-> [[2 0] [0 2]]

(inverse a) ; #<NDArrayDouble [[0.5 0.0] [0.0 0.5]]

(shape a) ;-> [2 2]

(dimensionality a)

(def M [[1 2 3]
        [4 5 6]
        [7 8 9]])

(det M) ;-> 6.661338147750939E-16

(pm (inverse M))

(slice M 1)

(apply + (slices M)) ; error

(use 'clojure.core.matrix.operators)

(apply + (slices M)) ;-> [12 15 18]

(+ 1 M) ;-> [[2 3 4] [5 6 7] [8 9 10]]
(+ [1 2 3] M) ;-> [[2 4 6] [5 7 9] [8 10 12]]
(+ (transpose [1 2 3]) M) ;-> [[2 4 6] [5 7 9] [8 10 12]]

(map inc M) ; error
(emap inc M) ;-> [[2 3 4] [5 6 7] [8 9 10]]
(ereduce * M) ;-> 362880

(def P (permutation-matrix [3 1 0 2])) 

(mmul P [1 2 3 4]) ; -> #<NDArray [4.0 2.0 1.0 3.0] 

(mmul (mmul P P) [1 2 3 4])

(mmul P P [1 2 3 4])

(mmul P P P [1 2 3 4]) ; #<NDArray [1.0 2.0 3.0 4.0] 

(eigen-decomposition P)

(cholesky-decomposition P)

(current-implementation)

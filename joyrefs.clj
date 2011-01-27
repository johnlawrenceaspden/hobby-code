(def initial-board
     [[:- :k :-]
      [:- :- :-]
      [:- :K :-]])

(defn do-board [f bd]
  (vec (map #(vec (for [s %] (f s))) bd)))

(do-board (constantly :*) initial-board)

(defn reset! []
  (def board (do-board ref initial-board))
  (def to-move (ref [[:K [2 1]] [:k [0 1]]]))
  (def num-moves (ref 0)))

(def king-moves (partial neighbors
                         [[-1 -1]  [-1 0] [-1 1] [0 -1] [0 1] [1 0] [1 1]] 3))

(defn good-move? [to enemy-sq]
  (when (not= to enemy-sq) to))

(defn choose-move [[[mover mpos][_ enemy-pos]]]
  [mover (some #(good-move? % enemy-pos)
               (shuffle (king-moves mpos)))])
                                                          
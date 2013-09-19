;; Union-Find I

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In the last post, I showed Kruskal's algorithm for finding Extremal
;; Spanning Trees in a weighted graph.

;; If we were to try to scale that algorithm, we'd find that it was
;; quadratic in the number of vertices in the graph.

;; The problem is that we're repeatedly searching lists of sets to see
;; whether two things are connected or not

;; So we could speed it up a lot if we could find a data structure
;; that is good at that sort of thing

;; Specifically, we'd like to be able to ask whether a is joined to b quickly,

;; and we'd like to be able to quickly modify the relation when we decide to join a to b

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Union-Find is a data structure specifically designed for keeping
;; track of partitions and equivalence relations.

;; A partition is a division of a large set into smaller sets
;; It can also be viewed as an equivalence relation

;; a is joined to b, if and only if a and be are in the same subset in the partition.

;; Consider our set of cities

(def cities ["London" "Birmingham" "Sheffield" "Bristol" "Leeds" "Liverpool" "Manchester"])

;; We'll make that into a hash of cities, where each city points to itself

(def initial (apply hash-map (mapcat (fn[x][x x]) cities)))

;; We'll interpret that map by saying 'All things that point to the same thing are in the same component'.
;; So in our initial map, where everything is pointing to itself, nothing is joined.

;; Say we want to assert that Liverpool ~ London. 
;; Then we want to make everything that points to Liverpool point to whatever it is London points to.

(defn join [union-find [a b]]
  (let [leader1 (union-find a)
        leader2 (union-find b)]
    (into {} (map (fn[[k v]] (if (= v leader1) [k leader2] [k v])) union-find))))

;; Let's connect Liverpool to London and London to Bristol, and Manchester to Sheffield
(reduce join initial [["Liverpool" "London"] ["London" "Bristol" ] ["Manchester" "Sheffield"]]) 
;-> {"Liverpool" "Bristol", "Sheffield" "Sheffield", "Manchester" "Sheffield", "Birmingham" "Birmingham", "Bristol" "Bristol", "London" "Bristol", "Leeds" "Leeds"}

;; Notice that Liverpool, Bristol, and London all point to Bristol (call that the Bristol Group)
;; And that Sheffield and Manchester form a group with Sheffield as its leader (call that the Sheffield group)
;; Whilst Leeds and Birmingham stand in splendid isolation, leaders of their own groups.

;; Now we can easily and quickly check which places are connected

(defn joined? [union-find a b]
  (= (union-find a)(union-find b)))

(joined? 
 (reduce join initial [["Liverpool" "London"] ["London" "Bristol" ] ["Manchester" "Sheffield"]])
 "Bristol" "Liverpool") ;-> true

(joined? 
 (reduce join initial [["Liverpool" "London"] ["London" "Bristol" ] ["Manchester" "Sheffield"]])
 "Bristol" "Manchester") ;-> false

;; But that's only half the problem. When joining cities, we still need to scan the whole map.

;; That means that if we're mainly joining things, rather than querying them, our performance is still poor.

;; So we should make each leader keep a list of the things that point to it.

;; Let's start again!























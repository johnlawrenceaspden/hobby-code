;; Union-Find I

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



































;; Kruskal's Algorithm and Minimal Spanning Trees

;; Suppose, in a wild flight of speculative fantasy, that years of conservative government
;; has completely destroyed the english railway network.

;; And piling absurdity upon absurdity, suppose that an eventual socialist government has
;; decided to rebuild it.

;; Under socialism, there's no money, so it's important to
;; connect the largest cities as cheaply as possible.
(def cities ["London" "Birmingham" "Sheffield" "Bristol" "Leeds" "Liverpool" "Manchester"])


;; Cadres of engineers are employed to pull cost projections out of their asses:
(def link-costs 
  [
   ["London" "Birmingham" 103]
   ["London" "Sheffield"  167]
   ["London" "Leeds" 175]
   ["London" "Bristol" 100]
   ["London" "Liverpool" 178]
   ["London" "Manchester" 181]

   ["Birmingham" "Sheffield"  91]
   ["Birmingham" "Leeds" 92 ]
   ["Birmingham" "Bristol" 79 ]
   ["Birmingham" "Liverpool" 75 ]
   ["Birmingham" "Manchester" 95]

   ["Sheffield" "Bristol" 180]
   ["Sheffield" "Leeds" 33]
   ["Sheffield" "Liverpool" 63]
   ["Sheffield" "Manchester" 37]

   ["Bristol" "Leeds" 171]
   ["Bristol" "Liverpool" 136]
   ["Bristol" "Manchester" 139]

   ["Leeds" "Liverpool" 73]
   ["Leeds" "Manchester" 40]

   ["Liverpool" "Manchester" 27]])

;; First we do two preprocessing steps:

;; We put the potential links in order of cost
(def links (sort-by (fn[[_ _ b]] b) link-costs))

;; And construct an initial partition of the cities
(def city-partition (map (comp set list) cities))

;; We need a function which can merge sets
(defn partition-merge [part a b]
  (conj (filter #(not (or (contains? % a)(contains? % b))) part)
        (apply clojure.set/union 
               (concat (filter #(contains? % a) part)
                       (filter #(contains? % b) part)))))

;; So for instance, should we build a route between London and Liverpool
(partition-merge city-partition "London" "Liverpool") 
;-> (#{"Liverpool" "London"} #{"Birmingham"} #{"Sheffield"} #{"Bristol"} #{"Leeds"} #{"Manchester"})

;; The partition-merge keeps track of the fact that we've connected the two

;; Note that if we build a redundant line, the size of the partition will not reduce
(-> city-partition
    (partition-merge "London" "Liverpool")
    (partition-merge "Bristol" "Liverpool")
    (partition-merge "London" "Bristol")) 
;-> (#{"Liverpool" "Bristol" "London"} #{"Birmingham"} #{"Sheffield"} #{"Leeds"} #{"Manchester"})


;; Kruskal has told us that should go through our list of links in cost order, building
;; only those which increase the connectivity.

;; This function takes the partition so far, and adds a new link to the tree it is building
;; if and only if the link helps
(defn add-link [[partition tree] link]
  (let [new (partition-merge partition (first link) (second link))]
    (if (< (count new) (count partition))
      [new (cons link tree)]
      [new tree])))

;; By the Power of Kruskal:

(def tree (second (reduce add-link [city-partition '()] links)))
;; ["London" "Bristol" 100]
;; ["Birmingham" "Bristol" 79]
;; ["Birmingham" "Liverpool" 75]
;; ["Sheffield" "Manchester" 37]
;; ["Sheffield" "Leeds" 33]
;; ["Liverpool" "Manchester" 27]

;; The people's railway should go 
;; London->Bristol->Birmingham->Liverpool->Manchester->Sheffield->Leeds

(reduce + (map (fn[[_ _ x]] x) tree)) ;-> 351

;; And it will use 351 glorious miles of shining steel.

;; Can we do better?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One way to do better, of course, if you were the head of the railway-building department
;; would be increase the size of your empire to the maximum you can get away with

;; Kruskal's Algorithm is equally good at finding maximal spanning trees

(def max-tree (second (reduce add-link [city-partition '()] (reverse links))))

(doseq [i max-tree] (println i))
;; [London Birmingham 103]
;; [Bristol Leeds 171]
;; [London Leeds 175]
;; [London Liverpool 178]
;; [Sheffield Bristol 180]
;; [London Manchester 181]

(reduce + (map (fn[[_ _ x]] x) max-tree)) ;-> 988

;; Now the railway goes in a star topology out from London to the great cities.

;; Ridiculous



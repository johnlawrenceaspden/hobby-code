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

(def tree (second (reduce add-link [city-partition '()] ordered)))
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





;; The Bellman Ford Algorithm

;; Breadth-first search gives us distances in graphs where all edges
;; are the same weight.

;; Dijkstra's Algorithm computed shortest distance in graphs with w,
;; but only when all edges are positive.

;; Sometimes our graphs have negative edge lengths. What can be done then?

;; Consider:

(def edges [[:A :B 5] 
            [:A :F 2]
            [:B :A -3] 
            [:B :E 2] 
            [:D :C 5] 
            [:E :D 5] 
            [:E :F 3] 
            [:E :G 5] 
            [:F :H -2] 
            [:G :F 10] 
            [:H :G 5] 
            [:H :I 5] 
            [:I :J 5] 
            [:J :L -20] 
            [:K :D 5] 
            [:L :K 5]])

(count edges) ;-> `17

(def in-edges (group-by second edges))

(def vertices (distinct (concat (map first edges) (map second edges))))

;; Distance from A to A is zero
(def initial {:A 0})

(in-edges :B) ;-> [[:A :B 5]]
(first (in-edges :B)) ;-> [:A :B 5]

(initial :A) ;-> 0
(initial :B) ;-> nil

(def new (assoc initial :B (+ 5 (initial :A)))) ;-> {:A 0, :B 5}

(in-edges :F) ;-> [[:A :F 2] [:E :F 3] [:G :F 10]]

(defn thingy[distmap [s d v]]
  (if-let [d ( initial s)]
    {:d (+ d v) :s s}))

(+ 2  (initial :A)) ;-> 0
(+ 3  (initial :E)) ;-> nil
(+ 10 (initial :G)) ;-> nil

(apply min-key :d (filter identity (map (partial thingy new) (in-edges :A) ))) ;-> {:d 2, :s :A}

(defn doit [distmap v]
  (let [poss (filter identity (map (partial thingy distmap) (in-edges v) ))]
    (if (empty? poss) distmap
        (let [{:keys[s d]} (apply min-key :d poss)]
          (assoc distmap v d)))))


(doit initial :A) ;-> {:A 0}
(doit initial :B) ;-> {:A 0, :B 5}
(doit initial :C) ;-> {:A 0}
(doit initial :D) ;-> {:A 0}

(def new (reduce doit initial vertices))

(reduce doit new vertices)
    
(iterate (fn[x] (reduce doit x vertices)) initial)






(let [s d w] (first (in-edges :B))
     (let [new (
     (if-let [cur (get initial s)]

;; Depth First Search and Strongly Connected Components

(require 'clojure.tools.reader.edn)

;; Here's a toy directed graph. Try drawing it and see if you can see any structure in it.
;; F -> C, G -> C, I -> E, C -> B, E -> H, B -> I, D -> A, F -> D, H -> I, B -> G, A -> F

;; Here's the same graph as it might be stored in a text file
(def graphstring
  ":F :C\n:G :C\n:I :E\n:C :B\n:E :H\n:B :I\n:D :A\n:F :D\n:H :I\n:B :G\n:A :F\n")

;; When we read it in, it would be nice to know:
;; What the nodes are, which nodes you can get to from a node, and which nodes you can get to a node from.

(defn add-edge [edgemap [from to]] 
  (conj edgemap [ from (conj (edgemap from #{}) to)]))

(defn make-graph 
  ([edge-seq] (make-graph edge-seq {} {} #{} 0))
  ([edge-seq edges revedges nodes counter]
     (when (zero? (mod counter 1000)) (println counter (System/nanoTime)))
     (if (empty? edge-seq) {:edges edges :revedges revedges :nodes nodes}
         (let [ns (first edge-seq)]
           (if (= (count ns) 2)
             (let [[a b] ns]
               (recur (rest edge-seq) 
                      (add-edge edges    [a b]) 
                      (add-edge revedges [b a]) 
                      (conj (conj nodes a) b)
                      (inc counter)))
             (do (print "failzor: " (first edge-seq) "->" ns)
                 (recur (rest edge-seq) edges revedges nodes counter)))))))

(defn graph-from-string [graph-string]
  (let [edge-lines (clojure.string/split graph-string #"\n")
        edge-pairs (for [l edge-lines] (clojure.string/split l #"\s"))
        edge-seq   (for [l edge-pairs] (map clojure.tools.reader.edn/read-string l))]
    (make-graph edge-seq)))


;; Our representation of it is edges forwards, edges backwards, list of nodes
(def small-graph (graph-from-string graphstring))

(clojure.pprint/pprint small-graph)
;; {:edges
;;  {:A #{:F},
;;   :H #{:I},
;;   :D #{:A},
;;   :B #{:G :I},
;;   :E #{:H},
;;   :C #{:B},
;;   :I #{:E},
;;   :G #{:C},
;;   :F #{:C :D}},
;;  :revedges
;;  {:F #{:A},
;;   :G #{:B},
;;   :D #{:F},
;;   :A #{:D},
;;   :I #{:B :H},
;;   :H #{:E},
;;   :B #{:C},
;;   :E #{:I},
;;   :C #{:F :G}},
;;  :nodes #{:A :C :B :F :G :D :E :I :H}}

;; The natural expression of depth-first search is as a recursion
;; which keeps a mutable list of visited nodes.

(defn rec-dfs 
  ([edges node] (rec-dfs edges node (atom #{})))
  ([edges node visited-set]
     (if (@visited-set node) @visited-set
         (do
           (swap! visited-set conj node)
           (doall (for [n (edges node)] (rec-dfs edges n visited-set)))
           @visited-set))))

;; Here's what we find if we dfs from various starting points

(rec-dfs (small-graph :edges) :A) ;-> #{:A :C :B :F :G :D :E :I :H}
(rec-dfs (small-graph :edges) :B) ;-> #{:C :B :G :E :I :H}
(rec-dfs (small-graph :edges) :C) ;-> #{:C :B :G :E :I :H}
(rec-dfs (small-graph :edges) :D) ;-> #{:A :C :B :F :G :D :E :I :H}
(rec-dfs (small-graph :edges) :E) ;-> #{:E :I :H}
(rec-dfs (small-graph :edges) :F) ;-> #{:A :C :B :F :G :D :E :I :H}
(rec-dfs (small-graph :edges) :G) ;-> #{:C :B :G :E :I :H}
(rec-dfs (small-graph :edges) :H) ;-> #{:E :I :H}
(rec-dfs (small-graph :edges) :I) ;-> #{:E :I :H}

;; The problem with this recursion is that it will blow stack on a big graph 
;; A lesser problem is that mutability is, if possible, to be avoided on ethical grounds

;; Here's a pure functional, iterative version of depth-first search
(defn iter-dfs 
  ( [edges node] (iter-dfs edges #{} (list node)))
  ( [edges visited-set to-visit-list]
      (if (empty? to-visit-list) visited-set
          (let [[node & rest] to-visit-list]
            (if (visited-set node) (recur edges visited-set rest)
                (recur edges
                       (conj visited-set node)
                       (concat (seq (edges node)) to-visit-list)))))))

;; This should be safe if we have enough memory to hold the graph and the visited-set.


;; Since the iterative version was easy to write but looks harder to
;; understand, I will permit myself a paranoid check.
(defn compare [edges nodes]
  (for [n nodes]
    (= (iter-dfs  edges n) (rec-dfs edges n))))

;; They produce the same set of visited nodes for a given starting node.
(compare (small-graph :edges) (small-graph :nodes)) ;-> (true true true true true true true true true)

;; Since the reversed graph is also a graph, this is valid too:
(compare (small-graph :revedges) (small-graph :nodes)) ;-> (true true true true true true true true true)


;; Let's look again at what's reachable from where:

(iter-dfs (small-graph :edges) :A) ;-> #{:A :C :B :F :G :D :E :I :H}
(iter-dfs (small-graph :edges) :B) ;-> #{:C :B :G :E :I :H}
(iter-dfs (small-graph :edges) :C) ;-> #{:C :B :G :E :I :H}
(iter-dfs (small-graph :edges) :D) ;-> #{:A :C :B :F :G :D :E :I :H}
(iter-dfs (small-graph :edges) :E) ;-> #{:E :I :H}
(iter-dfs (small-graph :edges) :F) ;-> #{:A :C :B :F :G :D :E :I :H}
(iter-dfs (small-graph :edges) :G) ;-> #{:C :B :G :E :I :H}
(iter-dfs (small-graph :edges) :H) ;-> #{:E :I :H}
(iter-dfs (small-graph :edges) :I) ;-> #{:E :I :H}

;; By inspection, if you start in EIH, you'll see EIH.
;; If you start in CBG, you'll see both CBG and EIH
;; and if you start in AFD, you'll see AFD, CBG and EIH, (the whole graph).

;; We call EIH, CBG, and AFD the strongly connected components.  

;; From any node in an scc, you can get to any other.  Some sccs are
;; 'downstream' of others, which means that you can get from one to
;; the other but not back.

;; We say that the sccs form a meta-graph: AFD -> CBG -> EIH. 

;; If we reverse the directions of the edges, then we'll get the same
;; sccs, but the edges in the meta-graph will be reversed.

(iter-dfs (small-graph :revedges) :A) ;-> #{:A :F :D}
(iter-dfs (small-graph :revedges) :B) ;-> #{:A :C :B :F :G :D}
(iter-dfs (small-graph :revedges) :C) ;-> #{:A :C :B :F :G :D}
(iter-dfs (small-graph :revedges) :D) ;-> #{:A :F :D}
(iter-dfs (small-graph :revedges) :E) ;-> #{:A :C :B :F :G :D :E :I :H}
(iter-dfs (small-graph :revedges) :F) ;-> #{:A :F :D}
(iter-dfs (small-graph :revedges) :G) ;-> #{:A :C :B :F :G :D}
(iter-dfs (small-graph :revedges) :H) ;-> #{:A :C :B :F :G :D :E :I :H}
(iter-dfs (small-graph :revedges) :I) ;-> #{:A :C :B :F :G :D :E :I :H}

;; By inspection, EIH -> CBG -> AFD in the reversed graph,
;; which is the same meta-graph only with all its edges reversed.

;; Notice that there are no cycles in the meta-graph. Can there ever be?

;; One thing we can do with our mutating recursion is to keep track of
;; when we are done with each node, which is to say, when we have
;; already visited every node to which it leads

(defn rec-dfs-with-finish-order 
  ([edges node] (rec-dfs-with-finish-order edges node (atom #{}) (atom '())))
  ([edges node visited-set finish-order]
     (when (not (@visited-set node)) 
       (swap! visited-set conj node)
       (doseq [n (edges node)] (rec-dfs-with-finish-order edges n visited-set finish-order))
       (swap! finish-order conj node))
     {:finish-order @finish-order :visited-set @visited-set }))

(rec-dfs-with-finish-order (small-graph :edges) :A) ;-> {:finish-order (:A :F :D :C :B :I :E :H :G), :visited-set #{:A :C :B :F :G :D :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :B) ;-> {:finish-order (:B :I :E :H :G :C), :visited-set #{:C :B :G :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :C) ;-> {:finish-order (:C :B :I :E :H :G), :visited-set #{:C :B :G :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :D) ;-> {:finish-order (:D :A :F :C :B :I :E :H :G), :visited-set #{:A :C :B :F :G :D :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :E) ;-> {:finish-order (:E :H :I), :visited-set #{:E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :F) ;-> {:finish-order (:F :D :A :C :B :I :E :H :G), :visited-set #{:A :C :B :F :G :D :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :G) ;-> {:finish-order (:G :C :B :I :E :H), :visited-set #{:C :B :G :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :H) ;-> {:finish-order (:H :I :E), :visited-set #{:E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :I) ;-> {:finish-order (:I :E :H), :visited-set #{:E :I :H}}

;; Call me Ishmael:
(defn compare2 [edges nodes]
  (for [n nodes]
    (let [{:keys [finish-order visited-set]} (rec-dfs-with-finish-order edges n)
          iter-vs (iter-dfs edges n)]
      (= (set finish-order) visited-set iter-vs))))

(compare2 (small-graph :edges) (small-graph :nodes)) ;-> (true true true true true true true true true)
(compare2 (small-graph :revedges) (small-graph :nodes)) ;-> (true true true true true true true true true)

;; We can compute a finish order for the whole graph if we dfs from every node, 
;; but preserve the visited-set and finish-orders between invocations
(defn ^:dynamic dfs-loop 
  ([edges node-order] (dfs-loop edges node-order (atom #{}) (atom '())))
  ([edges node-order visited-set finish-order]
    (cond (empty? node-order) {:finish-order @finish-order :visited-set @visited-set }
          (@visited-set (first node-order)) (recur edges (rest node-order) visited-set finish-order) 
          :else (let [{vs :visited-set fo :finish-order}
                      (rec-dfs-with-finish-order edges (first node-order) visited-set finish-order)]
                  (recur edges (rest node-order) (atom vs) (atom fo))))))


(dfs-loop (small-graph :edges) (seq (small-graph :nodes))) 
;-> {:finish-order (:A :F :D :C :B :I :E :H :G), :visited-set #{:A :C :B :F :G :D :E :I :H}}

;; Now, the clever bit is to compute this finish order for the reversed graph:
(:finish-order (dfs-loop (small-graph :revedges) (seq (small-graph :nodes))))
;-> (:E :I :H :C :G :B :A :D :F)

;; And then notice what that order does when we do dfs on the original graph

(rec-dfs (small-graph :edges) :E) ;-> #{:E :I :H}
(rec-dfs (small-graph :edges) :I) ;-> #{:E :I :H}
(rec-dfs (small-graph :edges) :H) ;-> #{:E :I :H}
(rec-dfs (small-graph :edges) :C) ;-> #{:C :B :G :E :I :H}
(rec-dfs (small-graph :edges) :G) ;-> #{:C :B :G :E :I :H}
(rec-dfs (small-graph :edges) :B) ;-> #{:C :B :G :E :I :H}
(rec-dfs (small-graph :edges) :A) ;-> #{:A :C :B :F :G :D :E :I :H}
(rec-dfs (small-graph :edges) :D) ;-> #{:A :C :B :F :G :D :E :I :H}
(rec-dfs (small-graph :edges) :F) ;-> #{:A :C :B :F :G :D :E :I :H}

;; It is in some sense the 'right' order for discovering the strongly connected components.

;; Let's see if we can take advantage of that, by passing on the visited set between calls:

(rec-dfs-with-finish-order (small-graph :edges) :E (atom #{}) (atom '())) ;-> {:finish-order (:E :H :I), :visited-set #{:E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :I (atom #{:E :I :H}) (atom '())) ;-> {:finish-order (), :visited-set #{:E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :H (atom #{:E :I :H}) (atom '())) ;-> {:finish-order (), :visited-set #{:E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :C (atom #{:E :I :H}) (atom '())) ;-> {:finish-order (:C :B :G), :visited-set #{:C :B :G :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :G (atom #{:C :B :G :E :I :H}) (atom '())) ;-> {:finish-order (), :visited-set #{:C :B :G :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :B (atom #{:C :B :G :E :I :H}) (atom '())) ;-> {:finish-order (), :visited-set #{:C :B :G :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :A (atom #{:C :B :G :E :I :H}) (atom '())) ;-> {:finish-order (:A :F :D), :visited-set #{:A :C :B :F :G :D :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :D (atom #{:A :C :B :F :G :D :E :I :H}) (atom '())) ;-> {:finish-order (), :visited-set #{:A :C :B :F :G :D :E :I :H}}
(rec-dfs-with-finish-order (small-graph :edges) :F (atom #{:A :C :B :F :G :D :E :I :H}) (atom '())) ;-> {:finish-order (), :visited-set #{:A :C :B :F :G :D :E :I :H}}

;; And so to compute the strongly connected components we can do
(defn strongly-connected-components [edges revedges node-order]
  (let [magic-order (:finish-order (dfs-loop revedges node-order))]
    (loop [magic-order magic-order partition '() visited-set #{}]
      (if (empty? magic-order) partition
          (let [{vs :visited-set fo :finish-order} 
                (rec-dfs-with-finish-order edges 
                  (first magic-order) 
                  (atom visited-set) 
                  (atom '()))]
            (if (empty? fo) 
              (recur (rest magic-order) partition vs)
              (recur (rest magic-order) (cons fo partition) vs)))))))

;; Behold:
(strongly-connected-components (small-graph :edges) (small-graph :revedges) (seq (small-graph :nodes))) 
;-> ((:A :F :D) (:C :B :G) (:E :H :I))

;; Notice that the order we choose might affect the various intermediate results,
;; and indeed the final result,  but not the partition into strongly connected components
(strongly-connected-components (small-graph :edges) (small-graph :revedges) (shuffle (seq (small-graph :nodes)))) 
;-> ((:F :D :A) (:B :G :C) (:H :I :E))
;-> ((:D :A :F) (:B :G :C) (:I :E :H))
;-> ((:A :F :D) (:B :G :C) (:E :H :I))
;-> ((:D :A :F) (:B :G :C) (:I :E :H))
;-> ((:F :D :A) (:B :G :C) (:H :I :E))
;-> ((:F :D :A) (:B :G :C) (:I :E :H))

;; Here's another graph, just to prove it wasn't a fluke.
;; A->B A->E B->C B->A  C->D C->I D->C E->F F->E F->I I->H H->G G->I

(def another-graph 
  (graph-from-string ":A :B\n:A :E\n :B :C\n:B :A\n:C :D\n:C :I\n:D :C\n:E :F\n:F :E\n:F :I\n:I :H\n:H :G\n:G :I\n"))

(strongly-connected-components (another-graph :edges) (another-graph :revedges) (shuffle (seq (another-graph :nodes)))) 
;-> ((:A :B) (:E :F) (:C :D) (:H :G :I))
;-> ((:D :C) (:B :A) (:E :F) (:G :I :H))
;-> ((:C :D) (:A :B) (:F :E) (:H :G :I))
;-> ((:D :C) (:A :B) (:F :E) (:H :G :I))










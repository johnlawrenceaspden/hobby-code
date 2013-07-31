(require 'clojure.tools.reader.edn)

(defn add-edge [edgemap [from to]] (conj edgemap [ from (conj (edgemap from #{}) to)]))

(defn make-graph 
  ([edge-seq] (make-graph edge-seq {} {} #{} 0))
  ([edge-seq edges revedges nodes counter]
     (when (zero? (mod counter 1000)) (println counter (System/nanoTime)))
     (if (empty? edge-seq) [edges revedges nodes]
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

;; Here's a graph
;; 154382697
;; ABCDEFGHI
(def graphstring
  ":F :C\n:G :C\n:I :E\n:C :B\n:E :H\n:B :I\n:D :A\n:F :D\n:H :I\n:B :G\n:A :F\n")

(def edge-lines (clojure.string/split graphstring #"\n"))

(def edge-pairs (for [l edge-lines] (clojure.string/split l #"\s")))

(def edge-seq   (for [l edge-pairs] (map clojure.tools.reader.edn/read-string l)))
  
;; Our representation of it is edges forwards, edges backwards, list of nodes
(def small-graph (make-graph edge-seq))

(clojure.pprint/pprint small-graph)

;; [{:A #{:F},
;;   :H #{:I},
;;   :D #{:A},
;;   :B #{:G :I},
;;   :E #{:H},
;;   :C #{:B},
;;   :I #{:E},
;;   :G #{:C},
;;   :F #{:C :D}}
;;  {:F #{:A},
;;   :G #{:B},
;;   :D #{:F},
;;   :A #{:D},
;;   :I #{:B :H},
;;   :H #{:E},
;;   :B #{:C},
;;   :E #{:I},
;;   :C #{:F :G}}
;;  #{:A :C :B :F :G :D :E :I :H}]


(def-let [[ edges revedges nodes ] small-graph])

nodes ;-> #{:A :C :B :F :G :D :E :I :H}
edges ;-> {:A #{:F}, :H #{:I}, :D #{:A}, :B #{:G :I}, :E #{:H}, :C #{:B}, :I #{:E}, :G #{:C}, :F #{:C :D}}
revedges ;-> {:F #{:A}, :G #{:B}, :D #{:F}, :A #{:D}, :I #{:B :H}, :H #{:E}, :B #{:C}, :E #{:I}, :C #{:F :G}}

;; How shall we do a search on such a graph?

;; Here's a functional, iterative version of depth-first search
(defn dfs [visited-set to-visit-list]
  (if (empty? to-visit-list) visited-set
      (let [[node & rest] to-visit-list]
        (if (visited-set node) (recur visited-set rest)
            (recur (conj visited-set node)
                   (concat (seq (edges node)) to-visit-list))))))

(dfs #{} '(:A)) ;-> #{:A :C :B :F :G :D :E :I :H}
(dfs #{} '(:B)) ;-> #{:C :B :G :E :I :H}
(dfs #{} '(:C)) ;-> #{:C :B :G :E :I :H}
(dfs #{} '(:D)) ;-> #{:A :C :B :F :G :D :E :I :H}
(dfs #{} '(:E)) ;-> #{:E :I :H}
(dfs #{} '(:F)) ;-> #{:A :C :B :F :G :D :E :I :H}
(dfs #{} '(:G)) ;-> #{:C :B :G :E :I :H}
(dfs #{} '(:H)) ;-> #{:E :I :H}
(dfs #{} '(:I)) ;-> #{:E :I :H}

;; Looks like the strongly connected components are EIH, CBG, and AFD
;; and the meta-graph is AFD -> CBG -> EIH. 

;; There's also a recursive way, but we need to mutate
;; the visited list as we recurse
(def visited-set (atom #{}))

(defn recdfs [node]
  (if (@visited-set node) 'done
      (do 
        (swap! visited-set conj node)
        (doall (map recdfs (edges node))))))

(defn nasty-dfs [node]
  (reset! visited-set  #{})
  (recdfs node)
  @visited-set) 

;; It works the same way as the iteration
(map nasty-dfs nodes) ;-> (#{:A :C :B :F :G :D :E :I :H} #{:C :B :G :E :I :H} #{:C :B :G :E :I :H} #{:A :C :B :F :G :D :E :I :H} #{:C :B :G :E :I :H} #{:A :C :B :F :G :D :E :I :H} #{:E :I :H} #{:E :I :H} #{:E :I :H})

;; One thing we can do with our mutating recursion is to keep track of when we are done with
;; each node.

(def visited-set (atom #{}))
(def finish-order (atom '()))

(defn rec-dfs-with-finish-order [node]
  (if (@visited-set node) 'done
      (do 
        (swap! visited-set conj node)
        (doall (map rec-dfs-with-finish-order (edges node)))
        (swap! finish-order conj node))))

(defn nasty-dfs-with-finish-order [node]
  (reset! visited-set  #{})
  (reset! finish-order  '())
  (rec-dfs-with-finish-order node)
  [@visited-set, @finish-order])

(nasty-dfs-with-finish-order :A) ;-> [#{:A :C :B :F :G :D :E :I :H} (:A :F :D :C :B :I :E :H :G)]
(nasty-dfs-with-finish-order :B) ;-> [#{:C :B :G :E :I :H} (:B :I :E :H :G :C)]
(nasty-dfs-with-finish-order :C) ;-> [#{:C :B :G :E :I :H} (:C :B :I :E :H :G)]
(nasty-dfs-with-finish-order :D) ;-> [#{:A :C :B :F :G :D :E :I :H} (:D :A :F :C :B :I :E :H :G)]
(nasty-dfs-with-finish-order :E) ;-> [#{:E :I :H} (:E :H :I)]
(nasty-dfs-with-finish-order :F) ;-> [#{:A :C :B :F :G :D :E :I :H} (:F :D :A :C :B :I :E :H :G)]
(nasty-dfs-with-finish-order :G) ;-> [#{:C :B :G :E :I :H} (:G :C :B :I :E :H)]
(nasty-dfs-with-finish-order :H) ;-> [#{:E :I :H} (:H :I :E)]
(nasty-dfs-with-finish-order :I) ;-> [#{:E :I :H} (:I :E :H)]


(defn dfs-with-order [node edges visited-set finish-order]
  (if (@visited-set node) 'done
      (do 
        (swap! visited-set conj node)
        (doseq [e (edges node)] (dfs-with-order e edges visited-set finish-order ))
        (swap! finish-order conj node))))

(defn dfs-loop [node-order edges]
  (let [visited-set  (atom #{})
        finish-order  (atom '())]
    (loop [no node-order]
      (if (empty? no) @finish-order
          (do
            (dfs-with-order (first no) edges visited-set finish-order)
            (recur (rest no)))))))


;; Notice how this expression has ordered the nodes by scc membership
(dfs-loop (dfs-loop (seq nodes) revedges) edges) ;-> (:A :F :D :C :B :G :E :H :I)

;; We can add labels as we dfs too

(defn dfs-with-order-and-labels [node edges visited-set finish-order label labels]
  (if (@visited-set node) 'done
      (do 
        (swap! visited-set conj node)
        (doseq [e (edges node)] (dfs-with-order-and-labels e edges visited-set finish-order label labels))
        (swap! labels conj [node label])
        (swap! finish-order conj node))))

(defn dfs-loop-with-order-and-labels [node-order edges]
  (let [visited-set  (atom #{})
        finish-order  (atom '())
        labels (atom {})]
    (loop [no node-order]
      (if (empty? no) [@finish-order @labels]
          (do
            (dfs-with-order-and-labels (first no) edges visited-set finish-order (first no) labels)
            (recur (rest no)))))))



(dfs-loop-with-order-and-labels (seq nodes) revedges) ;-> [(:A :F :D :C :B :I :E :H :G) {:A :A, :F :A, :D :A, :C :A, :B :A, :I :A, :E :A, :H :A, :G :A}]
(def labeled-sccs (second (dfs-loop-with-order-and-labels 
                            (first (dfs-loop-with-order-and-labels (seq nodes) revedges))
                            edges)))

(def sccs (for [a (partition-by second (seq labeled-sccs))] (map first a)))

(sort (map count sccs))





(require 'clojure.tools.reader.edn)

;; Here's a graph
;; 154382697
;; ABCDEFGHI
(def graphstring
  ":F :C\n:G :C\n:I :E\n:C :B\n:E :H\n:B :I\n:D :A\n:F :D\n:H :I\n:B :G\n:A :F\n")

(def edge-lines (clojure.string/split graphstring #"\n"))

(def edge-pairs (for [l edge-lines] (clojure.string/split l #"\s")))

(def edge-seq   (for [l edge-pairs] (map clojure.tools.reader.edn/read-string l)))
  
edge-seq ;; ((:F :C) (:G :C) (:I :E) (:C :B) (:E :H) (:B :I) (:D :A) (:F :D) (:H :I) (:B :G) (:A :F))

(defn add-edge [edgemap [from to]] (conj edgemap [ from (conj (edgemap from #{}) to)]))

(reduce add-edge {} [[1 2][:p "doom"][1 3]]) ;-> {:p #{"doom"}, 1 #{2 3}}

(defn make-graph [edge-seq edges revedges nodes counter]
  (when (zero? (mod counter 1000)) (println counter))
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
              (recur (rest edge-seq) edges revedges nodes counter))))))

(def small-graph (make-graph edge-seq {} {} #{} 0)) ;-> 
; [{:A #{:F}, :H #{:I}, :D #{:A}, :B #{:G :I}, :E #{:H}, :C #{:B}, :I #{:E}, :G #{:C}, :F #{:C :D}}
;  {:F #{:A}, :G #{:B}, :D #{:F}, :A #{:D}, :I #{:B :H}, :H #{:E}, :B #{:C}, :E #{:I}, :C #{:F :G}}
;  #{:A :C :B :F :G :D :E :I :H}]

;;(def scc (clojure.string/split-lines (slurp "SCC.txt")))
;;(def edge-pairs (for [l scc] (clojure.string/split l #"\s")))
;;(def edge-seq   (for [l edge-pairs] (map clojure.tools.reader.edn/read-string l)))
;;(take 10 edge-seq) ;-> ((1 1) (1 2) (1 5) (1 6) (1 7) (1 3) (1 8) (1 4) (2 47646) (2 47647))

(def sccgraph 
  (let [scc (clojure.string/split-lines (slurp "SCC.txt"))
        edge-pairs (for [l scc] (clojure.string/split l #"\s"))
        edge-seq   (for [l edge-pairs] (map clojure.tools.reader.edn/read-string l))]
    (make-graph edge-seq {} {} #{} 0)))

;; How to read such a big file
(time (with-open [rdr (clojure.java.io/reader "/home/john/hobby-code/SCC.txt")]
                       (count (line-seq rdr))))
;; 11 secs to read whole file and count the lines (not bad, since wc takes 17secs!)

(time (with-open [rdr (clojure.java.io/reader "/home/john/hobby-code/SCC.txt")]
        (let [scc (line-seq rdr)
              edge-pairs (for [l scc] (clojure.string/split l #"\s"))]
           (count edge-pairs))))
;; 80 secs to count the word pairs

(time (with-open [rdr (clojure.java.io/reader "/home/john/hobby-code/SCC.txt")]
        (let [scc  (line-seq rdr)
              edge-pairs (for [l scc] (clojure.string/split l #"\s"))
              edge-seq   (for [l edge-pairs] (map clojure.tools.reader.edn/read-string l))]
           (count edge-seq))))

;; 95 secs

(time (with-open [rdr (clojure.java.io/reader "/home/john/hobby-code/SCC.txt")]
        (let [scc  (line-seq rdr)
              edge-pairs (for [l scc] (clojure.string/split l #"\s"))
              edge-seq   (for [l edge-pairs] (map clojure.tools.reader.edn/read-string l))]
           (reduce + (map count edge-seq)))))

;; 95 secs

(time (def edge-seq 
        (with-open [rdr (clojure.java.io/reader "/home/john/hobby-code/SCC.txt")]
          (let [scc  (take 1000000 (line-seq rdr))
                edge-pairs (for [l scc] (clojure.string/split l #"\s"))
                edge-seq   (doall (for [l edge-pairs]
                                    (doall (map clojure.tools.reader.edn/read-string l))))]
           (doall edge-seq)))))

;; 199 seconds for 1000000, 16.6 secs for 100000 -> 1000 secs to read file

(time (reduce + (map count edge-seq)))
;; 0.7 secs for 100000, 7 secs for 1000000, -> 35 secs for whole file

(time (count (first (make-graph edge-seq {} {} #{} 0))))
;; 8 secs for 100000, 106 secs for 1000000 -> ~ 600secs for whole file)

(time (def scc-graph
        (with-open [rdr (clojure.java.io/reader "/home/john/hobby-code/SCC.txt")]
          (let [scc  (take 100000 (line-seq rdr))
                edge-pairs (for [l scc] (clojure.string/split l #"\s"))
                edge-seq   (doall (for [l edge-pairs]
                                    (doall (map clojure.tools.reader.edn/read-string l))))]
           (make-graph edge-seq {} {} #{} 0)))))

;; 23secs for 100000 -> 1000secs for whole thing ~ 20mins

(count (nth scc-graph 2))






(time (with-open [rdr (clojure.java.io/reader "/home/john/hobby-code/SCC.txt")]
        (let [scc (line-seq rdr)
              edge-pairs (for [l scc] (clojure.string/split l #"\s"))
              edge-seq   (for [l edge-pairs] (map clojure.tools.reader.edn/read-string l))]
           (count (first (make-graph (take 10000 edge-seq) {} {} #{} 0))))))

;; 2252msecs

(time (with-open [rdr (clojure.java.io/reader "/home/john/hobby-code/SCC.txt")]
        (let [scc (line-seq rdr)
              edge-pairs (for [l scc] (clojure.string/split l #"\s"))
              edge-seq   (for [l edge-pairs] (map clojure.tools.reader.edn/read-string l))]
           (count (first (make-graph (take 100000 edge-seq) {} {} #{} 0))))))

;; 24290 msecs. There are 5 million lines in our file so we're talking 30mins just to read them in!

(time (def scc-graph (with-open [rdr (clojure.java.io/reader "/home/john/hobby-code/SCC.txt")]
                       (let [scc (line-seq rdr)
                             edge-pairs (for [l scc] (clojure.string/split l #"\s"))
                             edge-seq   (for [l edge-pairs] (map clojure.tools.reader.edn/read-string l))]
                         (make-graph edge-seq {} {} #{} 0)))))



;; First let's look at our small example
(def-let [[ edges revedges nodes ] small-graph])

nodes ;-> #{:A :C :B :F :G :D :E :I :H}
edges ;-> {:A #{:F}, :H #{:I}, :D #{:A}, :B #{:G :I}, :E #{:H}, :C #{:B}, :I #{:E}, :G #{:C}, :F #{:C :D}}
revedges ;-> {:F #{:A}, :G #{:B}, :D #{:F}, :A #{:D}, :I #{:B :H}, :H #{:E}, :B #{:C}, :E #{:I}, :C #{:F :G}}

;; How shall we do a search on such a graph?

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




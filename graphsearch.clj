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




(def-let [[ edges revedges nodes ] small-graph])

nodes ;-> #{:A :C :B :F :G :D :E :I :H}
edges ;-> {:A #{:F}, :H #{:I}, :D #{:A}, :B #{:G :I}, :E #{:H}, :C #{:B}, :I #{:E}, :G #{:C}, :F #{:C :D}}
revedges ;-> {:F #{:A}, :G #{:B}, :D #{:F}, :A #{:D}, :I #{:B :H}, :H #{:E}, :B #{:C}, :E #{:I}, :C #{:F :G}}

(first nodes) ;-> :A
(edges :A) ;-> #{:F}
(revedges :A) ;-> #{:D}

;; How shall we do a search on such a graph?

(def visited #{})

;; look at the first node
(first nodes) ;-> :A

;; have we visited it?
(visited :A) ;-> nil

;; We have now
(def visited (conj visited :A))

(doseq [e (edges :A)] dfs 








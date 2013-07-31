(require 'clojure.tools.reader.edn)


(defn add-edge [edgemap [from to]] (conj edgemap [ from (conj (edgemap from #{}) to)]))

(defn make-edgehash [edge-seq edges counter reverse]
  (when (zero? (mod counter 1000)) (println counter (System/nanoTime)))
  (if (empty? edge-seq) edges
      (let [ns (first edge-seq)]
        (if (= (count ns) 2)
          (let [[a b] ns]
            (recur (rest edge-seq) 
                   (if reverse (add-edge edges [b a]) (add-edge edges [a b]) ) 
                   (inc counter)
                   reverse))
          (do (print "failzor: " (first edge-seq) "->" ns)
              (recur (rest edge-seq) edges counter reverse))))))


(defn read-edges [filename reverse] 
  (with-open [rdr (clojure.java.io/reader filename)]
    (make-edgehash
     (for [l (for [l (line-seq rdr)] 
               (clojure.string/split l #"\s"))] 
       (map clojure.tools.reader.edn/read-string l))
     {} 0 reverse)))

(defn read-reverse-edges [filename] (read-edges filename true))
(defn read-forward-edges [filename] (read-edges filename false))



(defn dfs [edges to-visit visited-set finished-list]
  (if (empty? to-visit) [visited-set finished-list]
      (let [[node & rst] to-visit]
        (if (visited-set node) (recur edges rst visited-set (cons node finished-list))
            (let [visited-set (conj visited-set node)
                  newedges (edges node)
                  new-to-visit (filter (comp not visited-set) newedges)]
              (if (empty? new-to-visit )
                (recur edges rst visited-set (cons node finished-list))
                (recur edges (concat new-to-visit to-visit) visited-set finished-list)))))))

(defn dfs-loop [edges node-order visited-set finished-list]
  (if (empty? node-order) finished-list
      (let [[start-node & rest] node-order]
        (if (visited-set start-node) (recur edges rest visited-set finished-list)
            (let [[visited-set finished-list] (dfs edges (list start-node) visited-set finished-list)]
              (recur edges rest visited-set finished-list))))))


(defn dfs-loop2 [edges node-order visited-set partition-list]
  (if (empty? node-order) partition-list
      (let [[start-node & rest] node-order]
        (if (visited-set start-node) (recur edges rest visited-set partition-list)
            (let [[visited-set finished-list] (dfs edges (list start-node) visited-set '())]
              (recur edges rest visited-set (cons finished-list partition-list)))))))


(def filename "/home/john/Desktop/SCC.txt")


(def reverse-edges (read-reverse-edges filename))

(def magic-order (dfs-loop reverse-edges (range 1 100) #{} '()))

(def reverse-edges nil)

(def forward-edges (read-forward-edges filename))
                                        
(def partition-list (dfs-loop2 forward-edges magic-order #{} '()))
 
(def partition-sizes (map count partition-list))

(take 10 (reverse (sort partition-sizes)))




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (time (def scc-edges ))

;; ;100          35
;; ;1000        331
;; ;10000      2217
;; ;100000    22907
;; ;5105000 1335183msec (22 mins)

;; (count scc-edges); 739454
;; (count (keys scc-edges)); 739454
;; (reduce max (keys scc-edges)) ;; 875714
;; (reduce min (keys scc-edges)) ;; 1

;; (time (count 
;;        (reduce 
;;         (fn[s [k v]] (conj (clojure.set/union s v) k))
;;         #{} 
;;         (take 10000000 scc-edges))))

;; ;; 10       102    4.9ms
;; ;; 100      723    18.37
;; ;; 1000     7009   116.8
;; ;; 10000    62688  1084msecs
;; ;; 100000   305491 8329msec
;; ;; 1000000  875714 76111msecs
;; ;; 10000000 875714 110941msecs
 
;; (time (reduce max (for [[k v] scc-edges] (count v))))
;; ;; -> 456 (14499 msecs)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

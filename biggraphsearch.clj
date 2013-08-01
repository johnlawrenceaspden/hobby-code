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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:dynamic iter-dfs-with-finish-order
  ( [edges to-visit-list visited-set finish-order] (iter-dfs-with-finish-order edges to-visit-list visited-set #{} finish-order))
  ( [edges to-visit-list visited-set pending-set finish-order]
      (if (empty? to-visit-list) {:finish-order finish-order :visited-set visited-set }
          (let [[node & rest] to-visit-list]
            (if (visited-set node) 
              ;; If it's been seen before we either ignore it, or move it from pending set to finished set
              (if (pending-set node) 
                (recur edges rest visited-set (disj pending-set node) (conj finish-order node)) 
                (recur edges rest visited-set pending-set finish-order))
              ;; If it's never been seen before, check its descendants
              (let [new-to-visit (filter #(not (visited-set %)) (edges node))]
                (if (empty? new-to-visit)
                  ;; and either put it straight on the finish list
                  (recur edges rest (conj visited-set node) pending-set (conj finish-order node))
                  ;; or mark it pending and put all its descendants on the to-visit-list
                  (recur edges (concat new-to-visit to-visit-list) (conj visited-set node) (conj pending-set node) finish-order))))))))

(defn iter-dfs-loop-1 
  ([edges node-order] (iter-dfs-loop-1 edges node-order #{} '()))
  ([edges node-order visited-set finish-order]
    (cond (empty? node-order) {:finish-order finish-order :visited-set visited-set }
          (visited-set (first node-order)) (recur edges (rest node-order) visited-set finish-order)
          :else (let [{vs :visited-set fo :finish-order}
                      (iter-dfs-with-finish-order edges (list (first node-order)) visited-set finish-order)]
                  (recur edges (rest node-order) vs fo)))))






(defn strongly-connected-components [edges revedges node-order]
  (let [magic-order (:finish-order (iter-dfs-loop revedges node-order))]
    (loop [magic-order magic-order 
           partition '() 
           visited-set #{}]
      (if (empty? magic-order) partition
          (let [{vs :visited-set fo :finish-order} 
                (iter-dfs-with-finish-order edges
                  (list (first magic-order)) visited-set '())]
            (if (empty? fo)
              (recur (rest magic-order) partition vs)
              (recur (rest magic-order) (cons fo partition) vs)))))))


(defn iter-dfs-loop-2 
  ([edges magic-order] (iter-dfs-loop-2 edges magic-order #{} '()))
  ([edges magic-order visited-set partition]
     (if (empty? magic-order) partition
         (let [{vs :visited-set fo :finish-order} 
               (iter-dfs-with-finish-order edges
                 (list (first magic-order)) visited-set '())]
           (if (empty? fo)
             (recur edges (rest magic-order) partition vs)
             (recur edges (rest magic-order) (cons fo partition) vs))))))

(def filename "/home/john/Desktop/SCC.txt")

(def reverse-edges (read-reverse-edges filename))

(def magic-order (iter-dfs-loop-1 reverse-edges (range 1 875714) #{} '()))

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

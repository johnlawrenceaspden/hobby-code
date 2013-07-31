(require 'clojure.tools.reader.edn)

(def filename "/home/john/Desktop/SCC.txt")

(defn add-edge [edgemap [from to]] (conj edgemap [ from (conj (edgemap from #{}) to)]))

(defn make-edgehash [edge-seq edges counter]
  (when (zero? (mod counter 1000)) (println counter (System/nanoTime)))
  (if (empty? edge-seq) edges
      (let [ns (first edge-seq)]
        (if (= (count ns) 2)
          (let [[a b] ns]
            (recur (rest edge-seq) 
                   (add-edge edges    [a b]) 
                   (inc counter)))
          (do (print "failzor: " (first edge-seq) "->" ns)
              (recur (rest edge-seq) edges counter))))))


(time (def scc-edges (with-open [rdr (clojure.java.io/reader filename)]
                       (make-edgehash
                        (for [l (for [l 
                                      (take 10000000 (line-seq rdr))] 
                                  (clojure.string/split l #"\s"))] 
                          (map clojure.tools.reader.edn/read-string l))
                        {} 0))))

;100          35
;1000        331
;10000      2217
;100000    22907
;5105000 1335183msec (22 mins)

(count scc-edges); 739454
(count (keys scc-edges)); 739454
(reduce max (keys scc-edges)) ;; 875714
(reduce min (keys scc-edges)) ;; 1

(time (count 
       (reduce 
        (fn[s [k v]] (conj (clojure.set/union s v) k))
        #{} 
        (take 10000000 scc-edges))))

;; 10       102    4.9ms
;; 100      723    18.37
;; 1000     7009   116.8
;; 10000    62688  1084msecs
;; 100000   305491 8329msec
;; 1000000  875714 76111msecs
;; 10000000 875714 110941msecs
 
(time (reduce max (for [[k v] scc-edges] (count v))))
;; -> 456 (14499 msecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


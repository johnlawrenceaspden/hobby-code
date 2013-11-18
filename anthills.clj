(require 'clojure.tools.reader.edn)

(defn parse-file [filename]
  (for [l (clojure.string/split-lines (slurp filename))] 
    (map clojure.tools.reader.edn/read-string (re-seq #"\S+" l))))

(def ants (parse-file "/home/john/Download/data1.txt"))

(require 'cemerick.pomegranate)
(cemerick.pomegranate/add-dependencies   
 :coordinates '[[incanter "1.5.4"]] :repositories {"clojars" "http://clojars.org/repo" } )

(use '(incanter core stats charts))

(view (scatter-plot (map first ants) (map second ants)))

(defn dot [v w] (reduce + (map * v w)))
(defn norm [v] (Math/sqrt (dot v v)))
(defn dist [a b] (norm (map - a b)))

(defn closest [point centroids]
  (first (sort-by (fn[[x y]] (dist point [x y])) centroids)))

(defn groups [centroids ants]
  (map second (group-by (fn [p] (closest p centroids)) ants)))

(defn meany [group]
  (let [c (count group)]
    (map #(/ % (float c)) (apply map + group))))

(defn it [centroids ants]
  (map meany
       (map second (group-by (fn [p] (closest p centroids)) ants))))

(def centroids [[0 0] [300 500] [500 1000]])

(map count (groups centroids ants))

(it [[0 0] [300 500] [1000 1000]] ants) 
;-> ((234.99 139.22) (419.6 887.20) (479.88 1000.0))
(it '[(234.99 139.22) (419.6 887.20) (479.88 1000.0)] ants)
;((234.36 144.415) (452.5050505050505 992.020202020202) (416.0 960.0))
(it '((234.36 144.415) (452.5050505050505 992.020202020202) (416.0 960.0)) ants)

(nth (iterate  (fn[c] (it c ants)) [[0 0] [300 500] [1000 1000]]) 10)



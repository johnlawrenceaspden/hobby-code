
;Manually starting an nrepl server
;(require 'cider.nrepl)
;(require 'clojure.tools.nrepl.server)
;(clojure.tools.nrepl.server/start-server :port 7888 :handler cider.nrepl/cider-nrepl-handler)

(defn coin [] (rand-nth [:H :T]))

(coin)
(def coins (take 10000 (iterate (fn[x](coin)) (coin))))

(def setsoffour (partition 4 1 coins))

(def freqs (frequencies setsoffour))

(freqs '(:T :T :T :T)) ; 43
(freqs '(:H :H :H :H)) ; 47

(freqs '(:H :H :H :T)) ; 61

(defn busfn[x]
  (cond (= x '(:H :H :H :H)) :red
        (= x '(:H :H :H :T)) :green
        :else '-))

(def buses (map busfn setsoffour))

coins ; (:H :H :T :T :T :H :T :T :T :H :H :T :H :H :H :T :H :T :T :H :T :T :H :T :T :H :H :T :T :H :T :T :T :H :T :H :H :H :T :H :T :T :T :H :H :H :T :H :H :T :H :T :T :H :T :H :T :T :H :T :H :T :T :T :T :H :T :T :T :T :T :T :H :T :H :H :H :T :H :H :T :H :H :T :H :T :T :H :T :H :T :H :T :T :H :H :H :T :H :T ...)
(drop 100 coins) ; (:T :H :H :H :T :T :T :T :H :H :H :H :H :H :T :H :T :T :T :H :T :H :T :T :T :H :T :H :H :H :T :T :T :H :T :H :T :T :T :T :T :T :T :T :H :H :H :T :H :H :T :T :H :T :H :T :T :H :H :T :H :T :H :T :T :H :T :H :T :T :T :H :T :H :H :H :H :H :H :T :H :T :H :H :H :T :T :T :T :H :T :T :H :H :H :T :T :T :T :H ...)
(filter identity buses) ; (- - - - - - - - - - - - :green - - - - - - - - - - - - - - - - - - - - - - :green - - - - - - - :green - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - :green - - - - - - - - - - - - - - - - - - - :green - - - - - ...)
(drop 100 (filter identity buses)) ; (- :green - - - - - - :red :red :red :green - - - - - - - - - - - - - - - :green - - - - - - - - - - - - - - - - :green - - - - - - - - - - - - - - - - - - - - - - - - - - - - :red :red :red :green - - - - - :green - - - - - - - - - :green - - - - - - - ...)
(drop 200 (filter identity buses)) ; (- - - - - - - - - - - - - - - :red :green - - - - - - - - - :green - - - - - - - - - - - :green - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - :red :red :green - - - - - - - - - - - - - - - - - - - - ...)

(def splits (partition-by #(= % '-)  buses))

(def timetable (map (fn[x] (if (= (first x) '-) (count x) x)) splits))

(sort-by second (frequencies timetable)) ;
([64 1]
 [47 1]
 [(:red :red :red :red :red :red :red :red :red :red :red :green) 1]
 [50 1]
 [(:red :red :red :red :red :red :red :red :red :red :green) 1]
 [83 1]
 [54 1]
 [56 1]
 [57 1]
 [33 2]
 [34 2]
 [40 2]
 [42 2]
 [55 2]
 [58 2]
 [60 2]
 [35 3]
 [36 3]
 [43 3]
 [46 3]
 [(:red :red :red :red :red :red :red :green) 3]
 [49 3]
 [29 3]
 [32 4]
 [37 4]
 [38 4]
 [41 4]
 [39 5]
 [23 5]
 [27 5]
 [30 6]
 [31 6]
 [22 8]
 [24 8]
 [28 8]
 [18 9]
 [21 9]
 [25 9]
 [26 9]
 [(:red :red :red :red :red :red :green) 10]
 [20 10]
 [19 11]
 [15 12]
 [(:red :red :red :red :red :green) 12]
 [11 17]
 [16 17]
 [13 18]
 [14 18]
 [17 23]
 [(:red :red :red :red :green) 26]
 [8 31]
 [10 31]
 [12 31]
 [6 34]
 [9 34]
 [4 35]
 [5 39]
 [7 39]
 [(:red :red :red :green) 42]
 [3 60]
 [(:red :red :green) 71]
 [(:red :green) 135]
 [(:green) 300])


;; Can look at this as a Markov Chain and ask about the waiting times

(use 'clojure.core.matrix)

(def A [[1 0 0 0 1 0 0 0]
        [1 0 0 0 1 0 0 0]
        [0 1 0 0 0 1 0 0]
        [0 1 0 0 0 1 0 0]
        [0 0 1 0 0 0 1 0]
        [0 0 1 0 0 0 1 0]
        [0 0 0 1 0 0 0 1]
        [0 0 0 1 0 0 0 1]])

(clojure.core.matrix/mmul A A)

(clojure.core.matrix/mmul A A A)

(use 'clojure.core.matrix.linear)

(set-current-implementation :clatrix)

(clojure.core.matrix.linear/svd A)

(pprint (clojure.core.matrix.linear/eigen A))

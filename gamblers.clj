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

(sort-by second (frequencies timetable))

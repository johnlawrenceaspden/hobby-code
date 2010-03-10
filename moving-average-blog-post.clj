;; Someone on Stack Overflow asked how to take the moving average of a list.

;; These were my answers:

;; The first one is elegant, but inefficient for long windows.
(defn average [lst] (/ (reduce + lst) (count lst)))
(defn moving-average [window lst] (map average (partition window 1 lst)))


;; The second is harder to understand, but keeps a running sum.
(defn partialsums [start lst]
  (lazy-seq
    (if-let [lst (seq lst)] 
          (cons start (partialsums (+ start (first lst)) (rest lst)))
          (list start))))

(defn sliding-window-moving-average [window lst]
  (map #(/ % window)
       (let [start   (apply + (take window lst))
             diffseq (map - (drop window lst) lst)]
         (partialsums start diffseq))))

;; Here is a list
(def integers (iterate inc 0))

;; Here are some averages of it:
(take 10 (moving-average 5 integers))
(take 10 (sliding-window-moving-average 5 integers))

;; Athena sprang fully-armed from the forehead of Zeus, and I have presented these functions in a similar spirit.

;; These two are not necessarily the fastest versions, but they are certainly the most elegant of the various versions
;; that I tried, and fairly careful benchmarking has not shown any of my other versions to be consistently faster.

;; If anyone is interested in the gory details, I have a long file of increasingly pretty attempts, 
;; stack blowing versions, tail recursive versions, timings etc, which could be turned into a post if there is demand.

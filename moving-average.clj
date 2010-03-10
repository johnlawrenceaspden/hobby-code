;; Someone on Stack Overflow asked how to take the moving average of a list

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



;; Just in case anyone is interested to see the gory details of the thought process:


;; There would seem to be an obvious improvement to be made by not counting the number of 
;; elements in the windows, since we already know it.
(defn average-2 [lst size] (/ (reduce + lst) size))
(defn moving-average-2 [window lst] (map #(average-2 % window) (partition window 1 lst)))

;; It occurs that folding the two functions into one might improve things.
(defn moving-average-3 [window lst] 
  (map #(/ (reduce +  %) window) (partition window 1 lst)))

;; And apply might work better than reduce?
(defn moving-average-4 [window lst] 
  (map #(/ (apply +  %) window) (partition window 1 lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Of course for long windows, our algorithm is doing lots of unnecessary work

;; What we actually want is a sliding window algorithm (window 3):
;; (0 1 2 3 4 5 6 7 8 9 10 11)
;;       (0 1 2 3 4 5 6 7  8 9 10 11)
;; ->
;; (+ 0 1 2) (3 3 3 3 3 3 3 3 3)
;; ->
;; 3 6 9 12 15 18 21 24 27
;; ->
;; 1 2 3 4 5 6 7 8 9

(defn sum [start lst]
  (lazy-seq
    (if-let [lst (seq lst)] 
      (let [n (+ start (first lst))]
          (cons n (sum n (rest lst))))
        '())))

(defn sliding-window-moving-average [window lst]
  (map #(/ % window)
       (let [start   (apply + (take window lst))
             diffseq (map - (drop window lst) lst)]
         (cons start (sum start diffseq)))))

;; This version is a bit faster, especially for long windows, since it keeps a rolling sum and avoids repeatedly adding the same things.
;; Because of the lazy-seq, it's also perfectly general and won't blow stack







;; Another way to phrase the same thing

(defn partialsums-3 [s as ms]
     (if (empty? as) (list s)
         (let [ns (- (+ s (first as)) (first ms))]
           (cons s (partialsums-3 ns (next as) (next ms))))))


(defn cunning-moving-average [window lst]
  (map #(/ % window) 
       (partialsums-3
        (reduce + (take window lst))
        (drop window lst)
        lst)))

;;This works fine
(cunning-moving-average 20 (take 100 (iterate inc 0)))
;;But blows the stack because it's not lazy
;; (take 100 (cunning-moving-average 20 (iterate inc 0)))
;; (cunning-moving-average 20 (take 3000 (iterate inc 0)))


(defn lazy-partialsums [s as ms]
  (lazy-seq
     (if (empty? as) (list s)
         (let [ns (- (+ s (first as)) (first ms))]
           (cons s (lazy-partialsums ns (next as) (next ms)))))))


(defn very-cunning-moving-average [window lst]
  (map #(/ % window) 
       (lazy-partialsums
        (reduce + (take window lst))
        (drop window lst)
        lst)))


;;This works fine
(very-cunning-moving-average 20 (take 100 (iterate inc 0)))
;;And no longer blows stack
(take 100 (very-cunning-moving-average 20 (iterate inc 0)))
(very-cunning-moving-average 20 (take 3000 (iterate inc 0)))


;; Apparently We can speed up the lazy sequence thing by calling seq on the sequences early
;; but in practice that doesn't seem to make much difference
(defn better-lazy-partialsums [s as ms]
  (lazy-seq
    (let [as (seq as)
          ms (seq ms)]
     (if as (let [ns (- (+ s (first as)) (first ms))]
           (cons s (lazy-partialsums ns (next as) (next ms))))
         (list s)))))

(defn exceedingly-cunning-moving-average [window lst]
  (map #(/ % window) 
       (better-lazy-partialsums
        (reduce + (take window lst))
        (drop window lst)
        lst)))

;; We can also write it as an imperative loop
(defn tail-recursive-moving-average [window lst]
  (map #(/ % window) (reverse 
                      (loop [sum (reduce + (take window lst))
                             addus (drop window lst)
                             subus lst
                             sofar (list sum)]
                        (if-let [addus (seq addus)]
                          (let [nsum (- (+ (first addus) sum) (first subus))]
                            (recur nsum (rest addus) (rest subus) (cons nsum sofar)))
                          sofar)))))
    

(def slow-flist   '(moving-average moving-average-2 moving-average-3 moving-average-4 ))
(def safe-flist   '(sliding-window-moving-average sliding-window-moving-average-2 very-cunning-moving-average exceedingly-cunning-moving-average tail-recursive-moving-average ))
(def unsafe-flist '( cunning-moving-average))


(= ; a test
 (range 2 18)
 (moving-average 5 (take 20 (iterate inc 0)))
 (moving-average-2 5 (take 20 (iterate inc 0)))
 (moving-average-3 5 (take 20 (iterate inc 0)))
 (moving-average-4 5 (take 20 (iterate inc 0)))
 (cunning-moving-average 5 (take 20 (iterate inc 0)))
 (very-cunning-moving-average 5 (take 20 (iterate inc 0)))
 (exceedingly-cunning-moving-average 5 (take 20 (iterate inc 0)))
 (sliding-window-moving-average 5 (take 20 (iterate inc 0)))
 (sliding-window-moving-average-2 5 (take 20 (iterate inc 0)))
 (tail-recursive-moving-average 5 (take 20 (iterate inc 0)))
)


(defn timings [flist window seq]
  (. (Runtime/getRuntime) gc)
  (for [f flist]
    (let [s (doall seq)
          start (System/nanoTime)
          r (doall (f window s))
          end (System/nanoTime)]
      (Math/round (/ (- end start) 1000000.0)))))



(defn sym->shortsym [sym]  (apply str ((fn[x] (cons (first x) (map second (re-seq #"-(.)" x)))) (str sym))))


(defn timing-chart [ windows lengths fns]
    (let [ flist        (map resolve fns)
           fabbs        (map sym->shortsym fns)]
      (for [ l lengths 
            w windows]
        (let [intlist      (take l integers)]
          [ w l (timings flist w intlist) ]))))

(require 'clojure.contrib.pprint)
(defn print-timing-chart [windows lengths fns]
  (clojure.contrib.pprint/cl-format true "~{ ~{ ~6d ~8d ~{ ~5d ~} ~} \n~}~%" 
             (cons (list 'window 'length (map sym->shortsym fns))
                   (timing-chart windows lengths fns))))
  

(print-timing-chart '(3 10 30 100) '(100 300 1000) (concat slow-flist safe-flist unsafe-flist))
(print-timing-chart '(3 4 5 6 7 8 9) '(100 300 1000 3000 10000 30000) (concat slow-flist safe-flist))

(print-timing-chart '(3 10 30 100 300 1000) '(100 300 1000 3000 10000 30000 100000) (concat safe-flist))



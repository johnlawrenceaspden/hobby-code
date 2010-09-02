;; A futile design process

;; The other day, I found myself wanting a running average of a list

(def integers (iterate inc 0))

;; At first, I took the easy way out by just calculating it

(defn runavg [sq]
     (fn [n]
       (/ (reduce + (take (inc n) sq)) (inc n))))

;; So here is the fifth running average
((runavg integers) 5)

;; After a while, I had occasion to make a list of these things
(def runavgs (map (runavg integers) (iterate inc 0)))

(take 5 runavgs)

;; this list proved very useful, but Schlemiel the Painter is at work here.

(time (count (take 100    (map (runavg integers) (iterate inc 0))))) ;;12ms
(time (count (take 1000   (map (runavg integers) (iterate inc 0))))) ;;1200ms
(time (count (take 10000  (map (runavg integers) (iterate inc 0))))) ;;120s
(time (count (take 100000 (map (runavg integers) (iterate inc 0))))) ;;3 hours

;; This wasn't a problem for my program as first written, but as I pushed it
;; this became the major bottleneck, and so I decided to optimize.








(reductions +   '(1 2 3 4 5))   ->   (1 3 6 10 15)
(reductions + 0 '(1 2 3 4 5)) ->   (0 1 3 6 10 15)
(take 5 (reductions + 0 (iterate inc 0)) ) (0 0 1 3 6)

(defn reduce-seq [f acc sq]
  (if (empty? sq) (list acc)
      (let [next-acc (f acc (first sq))]
        (cons acc (reduce-seq f next-acc (rest sq))))))

(reduce-seq + 0 '(1 2 3 4 5))   ->        (0 1 3 6 10 15)

(defn reduce-seq [f acc sq]
  (lazy-seq
   (if (empty? sq) (list acc)
       (let [next-acc (f acc (first sq))]
         (cons acc (reduce-seq f next-acc (rest sq)))))))

(take 5 (reduce-seq + 0 (iterate inc 0))) (0 0 1 3 6)
(reduce-seq + 0 '(1 2 3 4 5))   ->        (0 1 3 6 10 15)

(defn reduce-seq
  ([f acc sq]
     (lazy-seq
      (if (empty? sq) (list acc)
          (let [next-acc (f acc (first sq))]
            (cons acc (reduce-seq f next-acc (rest sq)))))))
  ([f sq] (reduce-seq f (first sq) (rest sq))))

(take 5 (reduce-seq + 0 (iterate inc 0))) (0 0 1 3 6)
(reduce-seq + 0 '(1 2 3 4 5))   ->        (0 1 3 6 10 15)
(reduce-seq +   '(1 2 3 4 5))   ->     (1 3 6 10 15)




(defn reduce-seq
  ([f acc sq]
     (lazy-seq
      (if-let [sq (seq sq)]
        (let [next-acc (f acc (first sq))]
          (cons acc (reduce-seq f next-acc (rest sq))))
        (list acc))))
  ([f sq] (reduce-seq f (first sq) (rest sq))))


(defn reduce-seq
  ([f acc sq]
     (lazy-seq
      (cons acc
            (if-let [sq (seq sq)]
              (let [next-acc (f acc (first sq))]
                (reduce-seq f next-acc (rest sq)))
              nil))))
  ([f sq] (reduce-seq f (first sq) (rest sq))))

(defn reduce-seq
  ([f acc sq]
     (lazy-seq
      (cons acc
            (if-let [sq (seq sq)]
              (let [next-acc (f acc (first sq))]
                (reduce-seq f next-acc (rest sq)))
              nil))))
  ([f sq] (reduce-seq f (first sq) (rest sq))))


(defn reduce-seq
  ([f acc sq]
     (lazy-seq
      (cons acc
            (if-let [sq (seq sq)]
              (let [next-acc (f acc (first sq))]
                (reduce-seq f next-acc (rest sq)))
              nil))))
  ([f sq] (reduce-seq f (first sq) (rest sq))))

(defn reduce-seq
  ([f acc sq]
     (lazy-seq
      (cons acc
            (when-let [sq (seq sq)]
                (reduce-seq f (f acc (first sq)) (rest sq))))))
  ([f sq] (reduce-seq f (first sq) (rest sq))))


(defn reduce-seq
  ([f acc sq]
     (cons acc
           (lazy-seq
            (when-let [sq (seq sq)]
              (reduce-seq f (f acc (first sq)) (rest sq))))))
  ([f sq] (if-let [sq (seq sq)]
            (reduce-seq f (first sq) (rest sq))
            (list (f)))))


(and
 ( = (reduce-seq +   '())    ,   '(0))
 ( = (reduce-seq + 1 '())    ,   '(1))
 ( = (reduce-seq +   '(1))   ,   '(1))
 ( = (reduce-seq + 1 '(1))   ,   '(1 2))
 ( = (take 5 (reduce-seq + 0 (iterate inc 0))), '(0 0 1 3 6))
 ( = (reduce-seq + 0 '(1 2 3 4 5)),             '(0 1 3 6 10 15))
 ( = (reduce-seq +   '(1 2 3 4 5)),             '(1 3 6 10 15))
 ( = (nth (reduce-seq + (iterate inc 0)) 10000), (* 1/2 10000 10001))
 ( = (reduce-seq #(assoc %1 %2 (inc (get %1 %2 0))) {} "aab"),
     '({} {\a 1} {\a 2} {\b 1, \a 2})))


(defn reductions
  "Returns a lazy seq of the intermediate values of the reduction (as
  per reduce) of coll by f, starting with init."
  {:added "1.2"}
  ([f coll]
     (lazy-seq
      (if-let [s (seq coll)]
        (reductions f (first s) (rest s))
        (list (f)))))
  ([f init coll]
     (cons init
           (lazy-seq
            (when-let [s (seq coll)]
              (reductions f (f init (first s)) (rest s)))))))

















;; I contend that this recursion 

(def regions (memoize (fn [dimensions planes]
  (cond (= dimensions 1) (list planes (inc planes))
        (= planes 0) (concat (repeat dimensions 0) '(1))
        :else (map + 
                   (regions dimensions (dec planes))
                   (concat '(0) (regions (dec dimensions) (dec planes))) 
                   (concat (regions (dec dimensions) (dec planes)) '(0)))))))

;; Gives us the number of objects of each dimension into which a
;; number of hyperplanes divide n-dimensional space.

(map #(regions 1 %) (range 10)) ;-> ((0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 8) (8 9) (9 10))
(map #(regions 2 %) (range 10)) ;-> ((0 0 1) (0 1 2) (1 4 4) (3 9 7) (6 16 11) (10 25 16) (15 36 22) (21 49 29) (28 64 37) (36 81 46))
(map #(regions 3 %) (range 10)) ;-> ((0 0 0 1) (0 0 1 2) (0 1 4 4) (1 6 12 8) (4 18 28 15) (10 40 55 26) (20 75 96 42) (35 126 154 64) (56 196 232 93) (84 288 333 130))

;; And I'd like to test that empirically.

;; My recursion tells me that n planes can divide space into
;; 2, 4, 8, 15, 26, and finally 42 regions.
(map last (map #(regions 3 %) (range 7))) ;-> (1 2 4 8 15 26 42)

;; But I'd like some sort of empirical confirmation of this result.

;; We've got some bits of geometry
(defn contract [form vector]
  (reduce + (map * form vector)))

(defn side [plane point]
  (- (contract (plane :form) point) (plane :val)))

(defn sign[x]
  (if (< x 0) '- '+))

(defn sig [point planes]
  (for [p planes] (sign (side p point))))

;; And some random sampling stuff
(defn cauchy[]
  (Math/tan (* Math/PI (- (rand) 0.5))))

(defn make-point []
  (repeatedly 3 cauchy))

(defn make-plane []
  {:form (repeatedly 3 cauchy) :val (cauchy)})


;; Putting it all together:
(defn sample-it [planes samples]
  (let [planes (repeatedly planes make-plane)
        points (repeatedly samples make-point)]
    (count (frequencies (map #(sig % planes) points )))))

;; It seems we have a valid method
(repeatedly 10 #(sample-it 0 1000)) ; (1 1 1 1 1 1 1 1 1 1)
(repeatedly 10 #(sample-it 1 1000)) ; (2 2 2 2 2 2 2 2 2 2)
;; which starts to miss regions even for easy cases
(repeatedly 10 #(sample-it 2 1000)) ; (4 4 4 3 4 4 4 4 4 4)
;; and which gets more unreliable quite quickly
(repeatedly 10 #(sample-it 3 1000)) ; (7 7 7 8 8 8 7 8 7 8)
;; until even with cases you can do in your head
(repeatedly 10 #(sample-it 4 1000)) ; (11 13 11 13 12 14 15 13 11 14)
;; the numbers can be quite badly off

;; We can throw more sampling at the problem
(repeatedly 10 #(sample-it 4 10000)) ; (13 14 13 14 15 14 13 14 12 14)

;; Causing it to improve
(repeatedly 10 #(sample-it 4 100000)) ; (15 15 15 15 15 15 15 14 13 11)

;; But the number of samples needed to get a good estimate scales badly with the number of planes
(repeatedly 10 #(sample-it 5 100000)) ; (25 22 25 25 24 25 24 20 25 24)


;; The traditional way of measuring the spread of a load of samples is:
(defn variance[lst]
  (let [mean (/ (reduce + lst) (count lst))
        devs (map #(- % mean) lst)
        sqdevs (map #(* % %) devs)]
    (/ (reduce + sqdevs) (count lst))))

(defn stddev [lst] (Math/sqrt (variance lst)))

(stddev '(2 2 2)) ; 0
(stddev '(4 2 0)) ; 1.632993161855452

;; If we look at the variance for the five-planes problem (simpler than we actually want to solve)
;; Then we find that it takes an awful lot of samples to get the variance down to the sort of level
;; where you'd expect to get the right answer every time

(stddev (repeatedly 10 #(sample-it 5 10))) ; 1.044030650891055 
(stddev (repeatedly 10 #(sample-it 5 100))) ; 2.9137604568666933 
(stddev (repeatedly 10 #(sample-it 5 1000))) ; 2.7129319932501073
(stddev (repeatedly 10 #(sample-it 5 10000))) ; 3.411744421846396
;(stddev (repeatedly 10 #(sample-it 5 100000))) ; 1.5132745950421556
;(stddev (repeatedly 10 #(sample-it 5 1000000))) ; 1.7888543819998317

;; Neither a faster language nor a faster computer will help us (much) here.

(stddev (repeatedly 10 #(sample-it 6 10))) ; 0.9797958971132712 
(stddev (repeatedly 10 #(sample-it 6 100))) ; 2.85657137141714 
(stddev (repeatedly 10 #(sample-it 6 1000))) ; 3.7161808352124095
(stddev (repeatedly 10 #(sample-it 6 10000))) ; 3.4871191548325386
;(stddev (repeatedly 10 #(sample-it 6 100000))) ; 1.4142135623730951 <- Oh look, the square root of two. Coincidence?
;(stddev (repeatedly 10 #(sample-it 6 1000000))) ; 1.1874342087037917

;; Looking at several runs, even with a million samples we haven't got the right answer once
(repeatedly 10 #(sample-it 6 1000000)) ;; (40 41 39 38 40 41 40 38 38 38)

;; My first thought is that picking the planes using the cauchy distribution 
;; may produce lots of small regions.

;; So what if we pick a likely-looking set of planes?
(defn sample-chosen-planes [planes samples]
  (let [points (repeatedly samples make-point)]
    (count (frequencies (map #(sig % planes) points )))))

;; The three coordinate axes are obvious choices
(def coordinate-planes (list
  {:form '(1 0 0), :val 0}
  {:form '(0 1 0), :val 0}
  {:form '(0 0 1), :val 0}))

;; And sampling works well to find the 8 octants that they divide space into
(repeatedly 10 #(sample-chosen-planes coordinate-planes 10)) ;-> (6 5 7 6 5 6 5 5 4 6)
(repeatedly 10 #(sample-chosen-planes coordinate-planes 100)) ; (8 8 8 8 8 8 8 8 8 8)

;; Sticking another one in
(def four-planes 
  (concat coordinate-planes
          (list  {:form '(1 1 1), :val 1})))

;; Seems to work pretty well
(repeatedly 10 #(sample-chosen-planes four-planes 10)) ; (6 7 8 7 6 9 6 6 5 9)
(repeatedly 10 #(sample-chosen-planes four-planes 100)) ; (13 14 15 15 14 14 14 13 15 14)
(repeatedly 10 #(sample-chosen-planes four-planes 1000)) ; (15 15 15 15 15 15 15 15 15 15)

;; So try the same trick again
(def five-planes 
  (concat coordinate-planes
          (list  {:form '(1 1 1), :val 1}
                 {:form '(1 1 -1), :val 1})))

;; Unfortunately
(repeatedly 10 #(sample-chosen-planes five-planes 100)) ; (21 19 20 20 21 20 19 21 20 21)
(repeatedly 10 #(sample-chosen-planes five-planes 1000)) ; (22 22 22 22 22 22 22 22 22 22)
(repeatedly 10 #(sample-chosen-planes five-planes 10000)) ; (22 22 22 22 22 22 22 22 22 22)
;; We now get unambiguous convergence to an answer that we're fairly sure is wrong

;; The recursion says that 5 planes can do 26 regions, and our chosen set apparently only does 22

;; Another go:
(def five-planes 
  (concat coordinate-planes
          (list  {:form '(1 1 1), :val 1}
                 {:form '(1 -1 -1), :val 1})))

;; produces a different, but higher wrong answer
(repeatedly 10 #(sample-chosen-planes five-planes 1000)) ; (24 24 24 24 24 24 24 24 24 24)

;; But putting a bit more thought into it
(def five-planes 
  (concat coordinate-planes
          (list  {:form '(1 1 1), :val 1}
                 {:form '(1 2 3), :val 0.5})))

(repeatedly 10 #(sample-chosen-planes five-planes 1000)) ; (22 22 22 23 24 22 23 24 24 23)
(repeatedly 10 #(sample-chosen-planes five-planes 10000)) ; (25 26 26 26 26 26 26 24 26 26)
(repeatedly 10 #(sample-chosen-planes five-planes 100000)) ; (26 26 26 26 26 26 26 26 26 26)

;; We can actually get the answer 26 that we're expecting.

;; But of course, it's perfectly possible that there's another combination of carefully chosen 
;; planes that produces more than 26 distinct regions. This is at least a lower bound.

;; As for the six plane case, it's hard to know how to choose the sixth plane
(def six-planes 
  (concat coordinate-planes
          (list  {:form '(1 1 1), :val 1}
                 {:form '(1 2 3), :val 0.5}
                 {:form '(4 5 7), :val 0.3})))

(repeatedly 10 #(sample-chosen-planes six-planes 100)) ; (19 19 22 21 24 24 24 21 19 20)
(repeatedly 10 #(sample-chosen-planes six-planes 1000)) ; (32 33 32 31 32 32 32 34 33 33)
(repeatedly 10 #(sample-chosen-planes six-planes 10000)) ; (38 39 36 37 37 37 38 38 36 37)
(repeatedly 10 #(sample-chosen-planes six-planes 100000)) ; (41 41 40 40 40 41 40 41 40 40)
(repeatedly 10 #(sample-chosen-planes six-planes 1000000)) ; (41 41 41 42 41 42 42 42 42 42)


;; The problem is that this looks to be settling down at 41, and we're
;; pretty sure that the answer's actually 42. 

;; So we push it a bit further, and lo and behold, it seems to struggle on to 42.

;; This is not a good way to run experiments. I'm getting the answer that I want from theory.

;; Luckily I currently have my hands on a gigantic 8 xeon server
;; so I can use clojure's easy concurrency to push this quite a lot
;; further than my poor little netbook would be able to go
(time (do
  (def doom (repeatedly 7 #(agent nil)))
  (print (map deref doom))
  (doseq [a doom] (send a (fn[a] (sample-chosen-planes six-planes 10000000))))
  (print (map deref doom))
  (doseq [a doom] (await a))
  (print (map deref doom))))

;; (nil nil nil nil nil nil nil)
;; (nil nil nil nil nil nil nil)
;; (42 42 42 42 42 42 42)
;; "Elapsed time: 485837.446802 msecs"

(time (do
  (def doom (repeatedly 7 #(agent nil)))
  (print (map deref doom))
  (doseq [a doom] (send a (fn[a] (sample-it 6 10000000))))
  (print (map deref doom))
  (doseq [a doom] (await a))
  (print (map deref doom))))

;; (nil nil nil nil nil nil nil)
;; (nil nil nil nil nil nil nil)
;; (39 42 39 41 41 42 42)
;; "Elapsed time: 706346.297653 msecs"

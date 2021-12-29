#!/usr/bin/env clojure

(set! *print-length* 103)
(set! *print-level* 13)

;; Fermats' Christmas Theorem: Fixed Points Come In Pairs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here's a bunch of code to make svg files of arrangements of coloured squares.
;; I'm using this to draw the windmills.
;; It's safe to ignore this if you're not interested in how to create such svg files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clojure.xml)

(def squaresize 10)

(defn make-svg-rect [i j colour]
  {:tag :rect
   :attrs {:x (str (* i squaresize)) :y (str (* j squaresize)) :width  (str squaresize) :height (str squaresize)
           :style (str "fill:", colour, ";stroke:black;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1")}})

(defn adjust-list [rectlist]
  (if (empty? rectlist) rectlist
      (let [hmin (apply min (map first  rectlist))
            vmax (apply max (map second rectlist))]
        (for [[a b c] rectlist] [(- a hmin) (- vmax b) c]))))

(defn make-svg [objects]
  {:tag :svg :attrs { :version "1.1"  :xmlns "http://www.w3.org/2000/svg"}
   :content (for [[i j c] (adjust-list objects)] (make-svg-rect i j c))})

(defn svg-file-from-rectlist [filename objects]
  (spit (str filename ".svg") (with-out-str (clojure.xml/emit (make-svg objects)))))

(defn hjoin
  ([sql1 sql2] (hjoin sql1 sql2 1))
  ([sql1 sql2 sep]
   (cond (empty? sql1) sql2
         (empty? sql2) sql1
         :else (let [xmax1 (apply max (map first sql1))
                     xmin2 (apply min (map first sql2))
                     shift  (+ 1 sep (- xmax1 xmin2))]
                 (concat sql1 (for [[h v c] sql2] [(+ shift h) v c]))))))

(defn hcombine [& sqllist] (reduce hjoin '() sqllist))

(defn svg-file [filename & objects]
  (svg-file-from-rectlist filename (apply hcombine objects)))

(defn orange [n] (if (< n 0) (range 0 n -1) (range 0 n 1)))

(defn make-composite-rectangle [h v hsquares vsquares colour]
  (for [i (orange hsquares) j (orange vsquares)] [(+ i h) (+ j v) colour]))

(defn make-windmill [[s p n]]
            (let [ s2 (quot s 2)
                  is2 (inc s2)
                  ds2 (- is2)]
              (concat (make-composite-rectangle  (- s2)  (- s2) s      s     "red")
                      (make-composite-rectangle  (- s2)  is2    p      n     "white")
                      (make-composite-rectangle  s2      ds2    (- p)  (- n) "white")
                      (make-composite-rectangle  ds2     (- s2) (- n)  p     "green")
                      (make-composite-rectangle  is2     s2     n      (- p) "green"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of drawing code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; and here's some code from the previous posts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; code to tell us what number a triple represents
(defn total [[s p n]] (+ (* s s) (* 4 p n)))

;; the easy green transform
(defn green [[s p n]] [s n p])

;; and the complicated red transform, which doesn't look so bad once you've got it.
(defn red [[s p n]]
  (cond (< p (/ s 2))   [(- s (* 2 p))       (+ n (- s p)) p]
        (< (/ s 2) p s) [(- s (* 2 (- s p))) p (+ n (- s p))]
        (< s p (+ n s)) [(+ s (* 2 (- p s))) p (- n (- p s))]
        (< (+ n s) p)   [(+ s (* 2 n))       n (- p (+ n s))]
        :else [s p n]))

;; Given a number of form 4n+1, make a 'thin cross' triple
(defn make-thin-cross [n] [1 1 (/ (- n 1) 4)])

;; Given a 'square bladed' triple, draw that as one odd and one even square
(defn victory [[s n p]]
  (assert (= n p))
  (hjoin
   (make-composite-rectangle 0 0 s s "red")
   (concat
    (make-composite-rectangle 0 0 n n "green")
    (make-composite-rectangle n n n n "green")
    (make-composite-rectangle 0 n n n "white")
    (make-composite-rectangle n 0 n n "white"))))


(defn factors [n] (for [i (range 1 (inc n)) :when (zero? (rem n i))] i))

(defn factor-pairs [n] (for [i (factors n)] [i (/ n i)]))

(defn odd-numbers-whose-square-is-less-than[n] (for [i (range 1 n 2) :while (< (* i i) n)] i))

(defn all-triples [m]
  (apply concat (for [s (odd-numbers-whose-square-is-less-than m)]
                  (for [[p n] (factor-pairs (/ (- m (* s s)) 4))]
                    [s p n]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of code from previous posts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Christmas Theorem is proved,

;; But I'm interested in these transforms for their own sake now, my questions are things like:

;; If n is a prime, and we start at a thin cross, then do we end up exploring the whole space of triples before
;; we get to the square bladed windmill?

;; If not, what does the rest of the space look like, can we get loops? are there green-green chains?

;; A green to green chain would imply two more ways to break the number down into sums of squares.
;; Are there prime numbers which can be represented in three different ways as sums of odd and even squares? Five? Seven? 

;; Here's a function which takes a triple, and follows it through the red-green-red-green cycle
;; until it goes somewhere it's been before (which must always happen because our space of triples is finite)

(defn calc-orbit-one-way
  ([triple transform] (calc-orbit triple transform (list triple) (set (list triple))))
  ([triple transform orbit visited]
   (let [new (transform triple)]
     ;(println triple transform "->" new orbit visited  )
     (if (visited new)
       (reverse orbit)
       (recur new (if (= transform green) red green) (cons new orbit) (conj visited new))))))


;; We can go in either direction and get different answers
(calc-orbit-one-way [1 1 2] green) ; ([1 1 2] [1 2 1])
(calc-orbit-one-way [1 1 2] red) ; ([1 1 2])

;; Although sometimes they are the same answer
(calc-orbit-one-way [1 1 1] green) ; ([1 1 1])
(calc-orbit-one-way [1 1 1] red) ; ([1 1 1])

;; Here's a slightly less trivial example
(calc-orbit-one-way [2 3 4] green) ; ([2 3 4] [2 4 3] [6 4 1] [6 1 4] [4 9 1] [4 1 9] [2 12 1] [2 1 12])
(calc-orbit-one-way [2 3 4] red) ; ([2 3 4] [4 3 3])

;; Since there's at least a theoretical possibility of looping, we can prime our second calculation with the result of the first calculation
;; Saying, add the new triples to the exisiting orbit, and stop whenever you get somewhere that we've been before
(def g234 (calc-orbit-one-way [2 3 4] green)) ; #'user/g234
(calc-orbit-one-way [2 3 4] red g234 (set g234)) ; ([2 1 12] [2 12 1] [4 1 9] [4 9 1] [6 1 4] [6 4 1] [2 4 3] [2 3 4] [4 3 3])


;; This gives us a function which will get the entire orbit of a triple
(defn calc-whole-orbit [triple]
  (let [go (calc-orbit-one-way triple green )
        ro (calc-orbit-one-way triple red  go (set go) )]
    ro))


(calc-whole-orbit [1 1 2]) ; ([1 2 1] [1 1 2])

(calc-whole-orbit [1 1 1]) ; ([1 1 1])

(calc-whole-orbit [2 3 4]) ; ([2 1 12] [2 12 1] [4 1 9] [4 9 1] [6 1 4] [6 4 1] [2 4 3] [2 3 4] [4 3 3])

(calc-whole-orbit [4 1 9]) ; ([4 3 3] [2 3 4] [2 4 3] [6 4 1] [6 1 4] [4 9 1] [4 1 9] [2 12 1] [2 1 12])

(calc-whole-orbit (make-thin-cross 85)) ; ([9 1 1] [7 9 1] [7 1 9] [5 15 1] [5 1 15] [3 19 1] [3 1 19] [1 21 1] [1 1 21])


(defn red-fixed-point? [triple] (= (red triple) triple))

(defn green-fixed-point? [triple] (= (green triple) triple))

(map red-fixed-point? (calc-whole-orbit (make-thin-cross 85))) ; (false false false false false false false false true)
(map green-fixed-point? (calc-whole-orbit (make-thin-cross 85))) ; (false false false false false false false false true)

(defn classify-triple [[s p n :as triple]]
  (cond (and (red-fixed-point? triple) (green-fixed-point? triple)) :dual-fixed-point
        (red-fixed-point? triple)                                   :red-fixed-point
        (green-fixed-point? triple)                                 :green-fixed-point
        :else                                                       :-))


(defn classify-triple [[s p n :as triple]]
  (cond (and (red-fixed-point? triple) (green-fixed-point? triple)) :dual-fixed-point
        ;;(red-fixed-point? triple)                                   :red-fixed-point
        (= s p 1)     :thin-cross
        (= s p)       :fat-cross
        (= (+ s n) p) :square
        (= n p)       :square-blades
        :else                                                       :-))

(defn add-flag [condition flag set]
  (if condition (conj set flag) set))



(defn classify-triple [[s p n :as triple]]
  (->> #{}
       (add-flag (= s p 1)  :thin-cross)
       (add-flag (and (= s p) (> s 1) ) :fat-cross)
       (add-flag (= (+ s n) p) :square)
       (add-flag (= n p) :square-blades)))

(calc-whole-orbit (make-thin-cross 85)) ; ([9 1 1] [7 9 1] [7 1 9] [5 15 1] [5 1 15] [3 19 1] [3 1 19] [1 21 1] [1 1 21])

(map classify-triple (calc-whole-orbit (make-thin-cross 85))) ; (#{:square-blades} #{} #{} #{} #{} #{} #{} #{} #{:fat-cross :thin-cross :red-fixed-point :cross})

(map classify-triple (calc-whole-orbit (make-thin-cross 85))) ; (:green-fixed-point :- :- :- :- :- :- :- :red-fixed-point)

(map classify-triple (calc-whole-orbit [1 18 2])) ; (:- :- :- :- :- :- :- :- :- :- :- :- :- :- :- :- :- :-)

(for [x (calc-whole-orbit [1 18 2])] (classify-triple x)) ; (:- :- :- :- :- :- :- :- :- :- :- :- :- :- :- :- :- :-)

(for [x (calc-whole-orbit [1 18 2]) :when (not= :- (classify-triple x))] (classify-triple x)) ; ()
(for [x (calc-whole-orbit (make-thin-cross 85)) :when (not= :- (classify-triple x))] (classify-triple x)) ; (:green-fixed-point :red-fixed-point)


(for [x (calc-whole-orbit (make-thin-cross 5)) :when (not= :- (classify-triple x))] (classify-triple x)) ; (:dual-fixed-point) ; (:dual-fixed-point)
(for [x (calc-whole-orbit (make-thin-cross 9)) :when (not= :- (classify-triple x))] (classify-triple x)) ; (:square :thin-cross) ; (:red-fixed-point :red-fixed-point)
(for [x (calc-whole-orbit (make-thin-cross 13)) :when (not= :- (classify-triple x))] (classify-triple x)) ; (:green-fixed-point :thin-cross) ; (:green-fixed-point :red-fixed-point)
(for [x (calc-whole-orbit (make-thin-cross 17)) :when (not= :- (classify-triple x))] (classify-triple x)) ; (:green-fixed-point :thin-cross) ; (:green-fixed-point :red-fixed-point)
(for [x (calc-whole-orbit (make-thin-cross 21)) :when (not= :- (classify-triple x))] (classify-triple x)) ; (:fat-cross :thin-cross) ; (:red-fixed-point :red-fixed-point)


(for [x (calc-whole-orbit (make-thin-cross 21))] (classify-triple x)) ; (#{:fat-cross} #{} #{} #{:thin-cross})
(for [x (calc-whole-orbit (make-thin-cross 21)) :when (not (empty? (classify-triple x)))] (classify-triple x)) ; (#{:fat-cross} #{:thin-cross})

(for [n (range 5 100 4)]
  [n (for [x (calc-whole-orbit (make-thin-cross n)) :when (seq (classify-triple x))] (classify-triple x))])








(defn classify-orbit [orbit]
  (let [h (first orbit)
        t (last orbit)]
        (cond
          (= h t) :point
          (and (red-fixed-point? h)   (red-fixed-point? t)  ) :red-red
          (and (green-fixed-point? h) (green-fixed-point? t)) :green-green
          (or (and (red-fixed-point? h) (green-fixed-point? t))
              (and (red-fixed-point? h) (green-fixed-point? t))):red-green
          :else :loop)))


 (defn classify-orbit [orbit]
  (let [h (first orbit)
        t (last orbit)
        rh (red-fixed-point? h)
        gh (green-fixed-point? h)
        rt (red-fixed-point? t)
        gt (green-fixed-point? t)]
    (cond (= h t) :point
          (and rh rt) :red-red
          (and gh gt) :green-green
          (or (and rh gt)
              (and rh gt)):red-green
          :else :loop)))
        


(classify-orbit (calc-whole-orbit (make-thin-cross 85))) ; :loop 
(classify-orbit (calc-whole-orbit [1 18 2])) ; :loop 






































(defn calc-orbit
  ([triple transform] (calc-orbit triple transform '() 100))
  ([triple transform orbit max]
   (let [new (transform triple)]
     (println new)
     (if (or (zero? max)( = new triple)) (reverse (cons triple orbit))
         (recur new (if (= transform green) red green) (cons triple orbit) (dec max))))))

(defn calculate-orbit
  ([triple transform] (calculate-orbit triple transform '()))
  ([triple transform orbit]
   (let [new (transform triple)]
     (if ( = new triple) (reverse (cons triple orbit))
         (recur new (if (= transform green) red green) (cons triple orbit))))))

(defn calc-whole-orbit [triple]
  (let [g (calc-orbit triple green )
        r (calc-orbit triple red   )]
    (assert (= (first g) (first r)))
    (concat (reverse (rest g)) r)))

(def naturals  (map inc (range)))
(def evens (map #(* 2 %) naturals))
(def odds  (map dec evens))
(defn prime? [n] (== (count (factors n)) 2))
(def primes (filter prime? naturals))
(def candidates (for [i naturals] (+ 1 (* 4 i))))
(def prime-candidates (filter prime? candidates))
(def non-prime-candidates (filter #(not (prime? %)) candidates))





























(make-thin-cross 25) ; [1 1 6]

(green [1 1 6]) ; [1 6 1]

(red [1 6 1]) ; [3 1 4]

(green [3 1 4]) ; [3 4 1]

(red [3 4 1]) ; [3 4 1]

;; Our orbit is:
(apply svg-file "windmill" (map make-windmill '([1 1 6] [1 6 1][3 1 4] [3 4 1])))

(defn calc-orbit
  ([triple transform] (calc-orbit triple transform '() 100))
  ([triple transform orbit max]
   (let [new (transform triple)]
     (println new)
     (if (or (zero? max)( = new triple)) (reverse (cons triple orbit))
         (recur new (if (= transform green) red green) (cons triple orbit) (dec max))))))

(calc-orbit [1 1 6] green ) ; ([1 1 6] [1 6 1] [3 1 4] [3 4 1])
(calc-orbit [1 1 6] red '() 100) ; ([1 1 6])


(apply svg-file "windmill" (map make-windmill (calc-orbit (make-thin-cross 49) green )))

(calc-orbit (make-thin-cross 49) green ) ; ([1 1 12] [1 12 1] [3 1 10] [3 10 1] [5 1 6] [5 6 1])

;; 49 = 1 + 4.12 12= 1*12, 3*4, 2*6

[1 1 12]
[1 2 6]
[1 3 4]
[1 4 3]
[1 6 2]
[1 12 1]

;; 49 = 3*3+40, 40 = 4*10, 10 = 1*10, 2*5

[3 1 10]
[3 2 5 ]
[3 5 2]
[3 10 1]

;; 49 = 5*5 + 24, 24 = 4*6 6= 1 2 3 6

[5 1 6]
[5 6 1]
[5 2 3]
[5 3 2]

;; 49 = 7*7


(calc-orbit [1 3 4] green ) ; ([1 3 4] [1 4 3])
(calc-orbit [1 3 4] red ) ; ([1 3 4] [5 3 2] [5 2 3] [1 6 2] [1 2 6] [3 2 5] [3 5 2])

(defn calc-whole-orbit [triple]
  (let [g (calc-orbit triple green )
        r (calc-orbit triple red   )]
    (assert (= (first g) (first r)))
    (concat (reverse (rest g)) r)))

(calc-whole-orbit [1 3 4]) ; ([1 4 3] [1 3 4] [5 3 2] [5 2 3] [1 6 2] [1 2 6] [3 2 5] [3 5 2])


(apply svg-file "windmill" (map make-windmill (calc-whole-orbit [1 3 4])))

(apply svg-file "windmill" (map make-windmill (calc-whole-orbit [3 4 4])))

(apply svg-file "windmill" (map make-windmill (calc-whole-orbit [1 1 3])))

(count (calc-whole-orbit [7 8 7])) ; 3 ; 2 ; 1 ; 28

(total [7 8 7]) ; 273





(defn factors [n] (for [i (range 1 (inc n)) :when (zero? (rem n i))] i))

(factors 12) ; (1 2 3 4 6 12)
(factors 100) ; (1 2 4 5 10 20 25 50 100)

(defn factor-pairs [n] (for [i (range 1 (inc n)) :when (zero? (rem n i))] [i (/ n i)]))

(factor-pairs 12) ; ([1 12] [2 6] [3 4] [4 3] [6 2] [12 1])

(defn odd-squares-less-than[n] (for [i (range 1 n 2) :while (< (* i i) n)] i))

(odd-squares-less-than 49) ; (0 1 2 3 4 5 6)

(defn all-triples [m]
  (apply concat (for [s (odd-squares-less-than m)]
                  (for [[p n] (factor-pairs (/ (- m (* s s)) 4))]
                    [s p n]))))

(def space49 (set (all-triples 49)))

(first space49) ; [1 1 12]
(calc-whole-orbit (first space49)) ; ([5 6 1] [5 1 6] [3 10 1] [3 1 10] [1 12 1] [1 1 12])

(clojure.set/difference space49 (set (calc-whole-orbit (first space49))))

(defn orbits [triples lst]
  (if (empty? triples) lst
      (let [o (calc-whole-orbit (first triples))]
        (recur (clojure.set/difference triples (set o)) (cons o lst)))))

(orbits space49 '()) ;
(([1 1 12] [1 12 1] [3 1 10] [3 10 1] [5 1 6] [5 6 1])
 ([3 5 2] [3 2 5] [1 2 6] [1 6 2] [5 2 3] [5 3 2] [1 3 4] [1 4 3]))


    
(orbits (set (all-triples 5)) '()) ; (([1 1 1]))
(orbits (set (all-triples 9)) '()) ; (([1 1 2] [1 2 1]))
(orbits (set (all-triples 13)) '()) ; (([1 1 3] [1 3 1] [3 1 1]))
(orbits (set (all-triples 17)) '()) ; (([1 1 4] [1 4 1] [3 1 2] [3 2 1] [1 2 2]))
(orbits (set (all-triples 21)) '()) ; (([3 3 1] [3 1 3] [1 5 1] [1 1 5]))
(orbits (set (all-triples 25)) '()) ; (([3 2 2] [1 2 3] [1 3 2]) ([1 1 6] [1 6 1] [3 1 4] [3 4 1]))
(orbits (set (all-triples 29)) '()) ; (([5 1 1] [3 5 1] [3 1 5] [1 7 1] [1 1 7]))
(orbits (set (all-triples 33)) '()) ; (([1 1 8] [1 8 1] [3 1 6] [3 6 1] [5 1 2] [5 2 1] [1 4 2] [1 2 4] [3 2 3] [3 3 2]))
(orbits (set (all-triples 37)) '()) ; (([1 1 9] [1 9 1] [3 1 7] [3 7 1] [5 1 3] [5 3 1] [1 3 3]))
(orbits (set (all-triples 41)) '()) ; (([1 1 10] [1 10 1] [3 1 8] [3 8 1] [5 1 4] [5 4 1] [3 4 2] [3 2 4] [1 2 5] [1 5 2] [5 2 2]))
(orbits (set (all-triples 45)) '()) ; (([3 3 3]) ([5 5 1] [5 1 5] [3 9 1] [3 1 9] [1 11 1] [1 1 11]))
(orbits (set (all-triples 49)) '()) ; (([1 1 12] [1 12 1] [3 1 10] [3 10 1] [5 1 6] [5 6 1]) ([3 5 2] [3 2 5] [1 2 6] [1 6 2] [5 2 3] [5 3 2] [1 3 4] [1 4 3]))
(orbits (set (all-triples 53)) '()) ; (([1 1 13] [1 13 1] [3 1 11] [3 11 1] [5 1 7] [5 7 1] [7 1 1]))
(map count (orbits (set (all-triples 53)) '())) ; (7)
(map count (orbits (set (all-triples 57)) '())) ; (16)

(make-thin-cross 57) ; [1 1 14]
(apply svg-file "windmill" (map make-windmill (calc-whole-orbit (make-thin-cross 57))))

(map count (orbits (set (all-triples 61)) '())) ; (16)
(apply svg-file "windmill" (map make-windmill (calc-whole-orbit (make-thin-cross 61))))
(factors 61) ; (1 61)

(map count (orbits (set (all-triples 65)) '())) ; (9 7)

(map count (orbits (set (all-triples 69)) '())) ; (10)
(apply svg-file "windmill" (map make-windmill (calc-whole-orbit (make-thin-cross 69))))

(map count (orbits (set (all-triples 73)) '())) ; (10)
(map count (orbits (set (all-triples 77)) '())) ; (8)

(for [i (range 35 40)] (map count (orbits (set (all-triples (+ 1 (* 4 i)))) '()))) ; ((18) (13 7 201) (15) (5 28) (19))

(map count (orbits (set (all-triples (+ 1 (* 4 36)))) '())) ; (13 7 201)

(orbits (set (all-triples 145)) '()) ;
(([1 6 6] [11 6 1] [11 1 6] [9 16 1] [9 1 16] [7 24 1] [7 1 24] [5 30 1] [5 1 30] [3 34 1] [3 1 34] [1 36 1] [1 1 36])
 ([5 5 6] [5 6 5] [7 6 4] [7 4 6] [1 4 9] [1 9 4] [9 4 4])
 ([1 18 2] [5 2 15] [5 15 2] [9 2 8] [9 8 2] [7 8 3] [7 3 8] [1 12 3] [1 3 12] [5 3 10] [5 10 3] [11 3 2] [11 2 3] [7 12 2] [7 2 12] [3 17 2] [3 2 17] [1 2 18] [1 18 2] [5 2 15] [5 15 2] [9 2 8] [9 8 2] [7 8 3] [7 3 8] [1 12 3] [1 3 12] [5 3 10] [5 10 3] [11 3 2] [11 2 3] [7 12 2] [7 2 12] [3 17 2] [3 2 17] [1 2 18] [1 18 2] [5 2 15] [5 15 2] [9 2 8] [9 8 2] [7 8 3] [7 3 8] [1 12 3] [1 3 12] [5 3 10] [5 10 3] [11 3 2] [11 2 3] [7 12 2] [7 2 12] [3 17 2] [3 2 17] [1 2 18] [1 18 2] [5 2 15] [5 15 2] [9 2 8] [9 8 2] [7 8 3] [7 3 8] [1 12 3] [1 3 12] [5 3 10] [5 10 3] [11 3 2] [11 2 3] [7 12 2] [7 2 12] [3 17 2] [3 2 17] [1 2 18] [1 18 2] [5 2 15] [5 15 2] [9 2 8] [9 8 2] [7 8 3] [7 3 8] [1 12 3] [1 3 12] [5 3 10] [5 10 3] [11 3 2] [11 2 3] [7 12 2] [7 2 12] [3 17 2] [3 2 17] [1 2 18] [1 18 2] [5 2 15] [5 15 2] [9 2 8] [9 8 2] [7 8 3] [7 3 8] [1 12 3] [1 3 12] [5 3 10] [5 10 3] [11 3 2] [11 2 3] [7 12 2] [7 2 12] [3 17 2] [3 2 17] [1 2 18] [1 18 2] [5 2 15] [5 15 2] [9 2 8] [9 8 2] [7 8 3] [7 3 8] [1 12 3] [1 3 12] [5 3 10] [5 10 3] [11 3 2] [11 2 3] [7 12 2] [7 2 12] [3 17 2] [3 2 17] [1 2 18] [1 18 2] [5 2 15] [5 15 2] [9 2 8] [9 8 2] [7 8 3] [7 3 8] [1 12 3] [1 3 12] [5 3 10] [5 10 3] [11 3 2] [11 2 3] [7 12 2] [7 2 12] [3 17 2] [3 2 17] [1 2 18] [1 18 2] [5 2 15] [5 15 2] [9 2 8] [9 8 2] [7 8 3] ...))

(calc-whole-orbit [1 18 2])

(set (calc-whole-orbit [1 18 2]))

(count (set (calc-whole-orbit [1 18 2]))) ; 18

(apply svg-file "windmill" (map make-windmill (set (calc-whole-orbit [1 18 2]))))

(calc-orbit [1 18 2] green '() 18) ; ([1 18 2] [1 2 18] [3 2 17] [3 17 2] [7 2 12] [7 12 2] [11 2 3] [11 3 2] [5 10 3] [5 3 10] [1 3 12] [1 12 3] [7 3 8] [7 8 3] [9 8 2] [9 2 8] [5 15 2] [5 2 15] [1 18 2])

;; our first infinite loop!
(apply svg-file "windmill" (map make-windmill (calc-orbit [1 18 2] green '() 18)))
(apply svg-file "windmill" (map make-windmill (calc-whole-orbit [1 6 6] )))
(apply svg-file "windmill" (map make-windmill (calc-whole-orbit [5 5 6] )))

(first (calc-whole-orbit [5 5 6] )) ; [9 4 4]
9*9 + 8*8
(+ (* 9 9) (* 8 8)) ; 145

(last (calc-whole-orbit [5 5 6])) ; [5 5 6]
(+ (* 5 5) (* 4 6 5)) ; 145
(* 5 (+ 5 (* 4 6))) ; 145
(* 5 (+ 5 24)) ; 145
(* 5 29) ; 145



(first (calc-whole-orbit [1 6 6] )) ; [1 6 6]
(+ (* 1 1) (* 12 12))

(last (calc-whole-orbit [1 6 6] )) ; [1 1 36]
(+ 1 (* 4 36)) ; 145

(factors 145) ; (1 5 29 145)

factors give
one fat cross and one thin cross
must be an even number of two-square decompositions

plus there's a loop with no fixed points


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of blog post
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note, to convert the svg files to png for posting on blogger, can do:
;; for i in windmills3*svg; do rsvg-convert -z 2 "$i" -o "${i%svg}png"; done

;; -z 2 (zoom factor 2 prevents the fuzzyness that affected the last post)


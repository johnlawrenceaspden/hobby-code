#!/usr/bin/env clojure

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

(defn make-thin-cross [n] [1 1 (/ (- n 1) 4)])

(defn victory [[s n p]]
  (assert (= n p))
  (hjoin
   (make-composite-rectangle 0 0 s s "red")
   (concat
    (make-composite-rectangle 0 0 n n "green")
    (make-composite-rectangle n n n n "green")
    (make-composite-rectangle 0 n n n "white")
    (make-composite-rectangle n 0 n n "white"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of code from previous posts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A Christmas Eve Theorem

;; Fixed Points Come In Pairs



;; For any given number of squares, there are only so many ways of arranging them into windmills

;; In fact it's easy to enumerate them all

;; Any given number has only so many factors
(defn factors [n] (for [i (range 1 (inc n)) :when (zero? (rem n i))] i))

(factors 12) ; (1 2 3 4 6 12)
(factors 100) ; (1 2 4 5 10 20 25 50 100)

;; So it only has so many pairs of factors
(defn factor-pairs [n] (for [i (range 1 (inc n)) :when (zero? (rem n i))] [i (/ n i)]))

(factor-pairs 12) ; ([1 12] [2 6] [3 4] [4 3] [6 2] [12 1])
(factor-pairs 36) ; ([1 36] [2 18] [3 12] [4 9] [6 6] [9 4] [12 3] [18 2] [36 1])

;; And there are only so many odd numbers that square to less than a given number
(defn odd-numbers-whose-square-is-less-than[n] (for [i (range 1 n 2) :while (< (* i i) n)] i))

(odd-numbers-whose-square-is-less-than 49) ; (1 3 5) 
(odd-numbers-whose-square-is-less-than 103) ; (1 3 5 7 9)

;; given these functions we can just produce all the possible triples
(defn all-triples [m]
  (apply concat (for [s (odd-numbers-whose-square-is-less-than m)]
                  (for [[p n] (factor-pairs (/ (- m (* s s)) 4))]
                    [s p n]))))

(all-triples 5) ; ([1 1 1])
(all-triples 9) ; ([1 1 2] [1 2 1])
(all-triples 13) ; ([1 1 3] [1 3 1] [3 1 1])
(all-triples 49) ; ([1 1 12] [1 2 6] [1 3 4] [1 4 3] [1 6 2] [1 12 1] [3 1 10] [3 2 5] [3 5 2] [3 10 1] [5 1 6] [5 2 3] [5 3 2] [5 6 1])


;; And these triples come in three kinds

;; The first kind is triples that are fixed points of both the red and green transform

;; [1 1 1] is such a triple

(red [1 1 1]) ; [1 1 1]
(green [1 1 1]) ; [1 1 1]

;; It's not connected to any other triple. It is a red fixed point and a green fixed point simultaneously.

;; The second kind is triples that are fixed points of one transform, but not the other

;; [1 1 2] is such a triple

(red   [1 1 2]) ; [1 1 2]
(green [1 1 2]) ; [1 2 1]

;; It's connected to one other triple, by the green transform. Let's say it's got a green connection
;; which goes somewhere.

;; [1 2 1] is also such a triple

(red [1 2 1]) ; [1 2 1]
(green [1 2 1]) ; [1 1 2]

;; Its green connection goes to [1 1 2], to form a two-triple chain with two red fixedpoints on it.

;; [3 1 1] is also such a triple
(green [3 1 1]) ; [3 1 1]
(red [3 1 1]) ; [1 3 1]

;; but it's a fixed point of the green transform, so it has a red connection going out, which goes to [1 3 1]

;; The third kind of triple isn't a fixed point of either transform

;; [1 3 1] is such a triple

(red [1 3 1]) ; [3 1 1]
(green [1 3 1]) ; [1 1 3]

;; It has two connections out, one red and one green,
;; And they must go to two different triples, because the red transform changes the size of the red square
;; and the green one doesn't

;; Most triples are of this third kind, they can form links in chains

;; An even number of them can potentially form a loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Suppose we start off from the first kind of triple.

;; We immediately find two fixed points, one red and one green, and we're done

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Suppose we start off from the second kind of triple

;; It will have a link out

;; If we follow that link, then we can't go to a triple of the first kind, because that hasn't got any connections

;; If we go to a triple of the second kind, then we're done, because we'll use up its only
;; connection, and we've formed a complete chain with two fixed points

;; If we go to a triple of the third kind, then we have two triples, and one free connection out

;; Where can that connection go?

;; Not back into the chain we're making, all the connections of those triples are already accounted for.

;; So it must go to another triple and we're in the same situation.

;; We can never have more than one free connection on our chain

;; And we can't go on for ever, because there are only so many triples.

;; So eventually we have to link to another of the first kind of triple, having built a chain that connects
;; two fixed points.

;; A chain that connects two red or two green fixed points must have an odd number of connections
;; A chain that connects a red fixed point to a green fixed point must have an even number of connections

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If we start from the third kind of triple, we've got two free connections, and we can attach triples to both of them.

;; But we never get more than two free connections.

;; We can't go on for ever, so we either have to join our two free connections to make a loop, or we
;; have to be part of a chain which connects two fixed points of type 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Christmas Eve Theorem

;; Fixed points come in pairs, if you start off from one and follow your links, you'll find another

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This immediately implies Fermat's Christmas Theorem

;; Because for every number of the form 4n+1

;; We can make one, and only one, thin cross, a red fixed point

;; The existence of this thin cross implies and is implied by the fact that the number is divisible by one.

;; It can't be part of a loop, by the argument above. And its chain can't go on for ever.

;; So if we follow the transforms, red, green, red, green, red, green ......

;; Sooner or later we'll hit another fixed point.

;; That other fixed point will be one of:

;; A square

;; A fat cross

;; or

;; A square-bladed windmill

;; If we find a square-bladed windmill, we've also found a way to write our number as the sum of an odd and an even square.

;; If we find a square, then we've shown that our number is itself an odd square

;; And if we find an fat cross, then we've shown that our number has a factorization, of the form s*(s+4n) for some n

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Another way of saying that is that although this program is not generally safe to run on arbitrary triples:

(defn calculate-orbit
  ([triple transform] (calc-orbit triple transform '()))
  ([triple transform orbit]
   (let [new (transform triple)]
     (println new)
     (if ( = new triple) (reverse (cons triple orbit))
         (recur new (if (= transform green) red green) (cons triple orbit))))))

;; Because it might go into an infinite loop


;; This program *is* safe to run, because starts at a red fixed point and so it must follow a chain and end at another fixed point
(defn christmas [n]
  (calculate-orbit (make-thin-cross n) green))

;; Let's try it out

(christmas 5) ; ([1 1 1])

(apply svg-file "windmill" (map make-windmill (christmas 5)))

;; Here we show that 5 = 2*2 + 1*1

(fermat-christmas 9) ; ([1 1 2] [1 2 1])

(apply svg-file "windmill" (map make-windmill (christmas 9))) ; nil

;; 9 = 3*3

(fermat-christmas 85) ; ([1 1 21] [1 21 1] [3 1 19] [3 19 1] [5 1 15] [5 15 1] [7 1 9] [7 9 1] [9 1 1])

(apply svg-file "windmill" (map make-windmill (christmas 85)))

;; 85 = 9*9+2*2    (even though it's not prime!)

(fermat-christmas 201) ; ([1 1 50] [1 50 1] [3 1 48] [3 48 1] [5 1 44] [5 44 1] [7 1 38] [7 38 1] [9 1 30] [9 30 1] [11 1 20] [11 20 1] [13 1 8] [13 8 1] [3 8 6] [3 6 8] [9 6 5] [9 5 6] [1 5 10] [1 10 5] [11 5 4] [11 4 5] [3 12 4] [3 4 12] [5 4 11] [5 11 4] [13 4 2] [13 2 4] [9 15 2] [9 2 15] [5 22 2] [5 2 22] [1 25 2] [1 2 25] [3 2 24] [3 24 2] [7 2 19] [7 19 2] [11 2 10] [11 10 2] [9 10 3] [9 3 10] [3 16 3] [3 3 16])

(apply svg-file "windmill" (map make-windmill (christmas 201)))

;; 201 = 3*(3 + 4*16) = 3*67

;; You might need to zoom in a bit on that last one, but the program will always work

;; Any number of the form 4n+1 can be either expressed as an odd square plus and even square, or it can be factored.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Merry Christmas Everybody!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of blog post
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note, to convert the svg files to png for posting on blogger, can do:
;; for i in windmills5*svg; do rsvg-convert -z 2 "$i" -o "${i%svg}png"; done

;; -z 2 (zoom factor 2 prevents the fuzzyness that affected the last post)


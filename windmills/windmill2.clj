#!/usr/bin/env clojure

;; Fermats' Christmas Theorem: Principled Windmills

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of drawing code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's do another example in a more principled way

;; We'll think in terms of triples [s, p, n]

;; Where s is the size of the red square, p (parallel) is the width of the arms , and n (normal) is
;; the length of the arms.

;; Here's a function to draw the windmill that represents such a triple

(defn make-windmill [[s p n]]
            (let [ s2 (quot s 2)
                  is2 (inc s2)
                  ds2 (- is2)]
              (concat (make-composite-rectangle  (- s2)  (- s2) s      s     "red")
                      (make-composite-rectangle  (- s2)  is2    p      n     "white")
                      (make-composite-rectangle  s2      ds2    (- p)  (- n) "white")
                      (make-composite-rectangle  ds2     (- s2) (- n)  p     "green")
                      (make-composite-rectangle  is2     s2     n      (- p) "green"))))

(make-windmill [1 1 1]) ; ([0 0 "red"] [0 1 "white"] [0 -1 "white"] [-1 0 "green"] [1 0 "green"])

(svg-file "windmill" (make-windmill [1 1 1]))

;; As we do our windmill transformations s*s + 4 * p * n should always stay the same
(defn total [[s p n]]
  (+ (* s s) (* 4 p n)))

(total [1 1 1]) ; 5



;; So with this new way of representing things:

;; Consider 37 = 4 * 9 + 1

;; Our first triple will be

[1 1 9]

(total [1 1 9]) ; 37

;; And its windmill looks like:
(svg-file "windmill-37-1" (make-windmill [1 1 9]))

;; We can't change the size of the red square here, so the other thing we can do is to rotate the arms

;; In terms of triples, [1 1 9] -> [1 9 1]

(total [1 9 1]) ; 37

(svg-file "windmill-37-2" (make-windmill [1 9 1]))

;; Now we can change the size of the red square, it can increase to three, and that means that we have to shorten the arms by two
;; [1 9 1] -> [3 1 7]

(total [3 1 7]) ; 37

(svg-file "windmill-37-3" (make-windmill [3 1 7]))

;; Note that this also changes the colour of the arms, but that doesn't matter, the only reason the
;; arms are two different colours is to make it easier to see what's going on.  If it bothers you
;; just go and change white to green in the windmill code!


;; From [3 1 7] the only change we can make to the size of the red square is to put it back to one
;; So instead, we'll swap the arms again
;; [3 1 7] -> [3 7 1]

(total [3 7 1]) ; 37

(svg-file "windmill-37-4" (make-windmill [3 7 1]))

;; Now, swapping the arms just moves us back a step, but we can increase the size of the red square to five
;; and shorten the arms by two
;; [3 7 1] -> [5 1 3]

(total [5 1 3]) ; 37

(svg-file "windmill-37-5" (make-windmill [5 1 3]))

;; Again, the only way to change the size of the red square is to put it back, so let's rotate arms

;; I'm going to call changing the size of the red square the red transformation
;; and rotating the arms the green transformation

;; The green transformation is easy to express in terms of triples
(defn green [[s p n]] [s n p])

(green [5 1 3]) ; [5 3 1]

(total [5 3 1]) ; 37

(svg-file "windmill-37-6" (make-windmill [5 3 1]))

;; Now, the green transformation just puts us back a step, and it looks like we can't increase the size
;; of the red square, so are we stuck?

;; No! If you stare at the diagram for long enough, you'll see that we can *reduce* the size of the
;; red square instead of increasing it, growing the arms inward until the red shape is square again.

;; And in fact that's our only possible move.

;; [5 3 1] -> [1 3 3]

(svg-file "windmill-37-7" (make-windmill [1 3 3]))

;; It's kind of annoying that this flips the shape! But it's obviously still the same total number
;; of squares, so just like with the colour flip I'm going to ignore that for now rather than
;; introduce unnecessary complexity to the drawing code

;; I'm going to call both reducing and increasing the size of the red square a "red transformation",
;; and the red transformation is going to need a parameter to say how much to change the size of the
;; square

;; Let's say, as above, that we want to shift the boundaries of the big red square in by two small unit squares

;; so say delta = -2

;; that means that the new red square is size one, five less two squares on either edge

;; that means that the red square has changed from size twenty-five to size one

;; that leaves twenty-four spare squares, to be distributed between the four arms

;; which is six spare squares per arm

;; since we're just moving the boundary of the square, or alternatively extending the arms into the
;; square, that doesn't change p, the width of the arm parallel to the square

;; so we add the six squares in rows of p.

;; in our example above, p is three, so those six squares result in the arms lengthening by two

;; In code

(defn red [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm p)]
    [news (+ n lengthchange) p]))

(total [5 3 1]) ; 37
(red [5 3 1] -2) ; [1 3 3]
(total (red [5 3 1] -2)) ; 37

(svg-file "windmill" (make-windmill [5 3 1 ])) ; nil
(svg-file "windmill" (make-windmill (red [5 3 1] -2))) ; nil


(svg-file "windmill-37-7" (make-windmill [1 3 3]))


;; Now we have a real problem. The only red transformation we can make is to go back a step, but
;; the green transformation does nothing useful here:

(green [1 3 3]) ; [1 3 3]


;; But every problem is an opportunity, as they say:

;; We can split up the rectangle
(svg-file "windmill-37-split"
          (make-composite-rectangle 0 0 1 1 "red")
          (make-composite-rectangle 0 0 3 3 "green")
          (make-composite-rectangle 0 0 3 3 "green")
          (make-composite-rectangle 0 0 3 3 "white")
          (make-composite-rectangle 0 0 3 3 "white"))

(svg-file "windmill-37-recombine"
          (make-composite-rectangle 0 0 1 1 "red")
          (concat
           (make-composite-rectangle 0 0 3 3 "green")
           (make-composite-rectangle 3 3 3 3 "green")
           (make-composite-rectangle 0 3 3 3 "white")
           (make-composite-rectangle 3 0 3 3 "white")))

;; The green transformation fails if and only if the arms are squares, and if the arms are squares, we can
;; combine them to form one big even square

(svg-file "windmill-37-final"
          (make-composite-rectangle 0 0 1 1 "red")
          (concat
           (make-composite-rectangle 0 0 6 6 "green")))

;; So 37 = 1*1 + 6*6

;; Which is what we're trying to show, thirty-seven is the sum of one odd and one even square


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of blog post
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note, to convert the svg files to png for posting on blogger, can do:
;; for i in windmill-37*svg; do rsvg-convert -z 2 "$i" -o "${i%svg}png"; done

;; -z 2 (zoom factor 2 prevents the fuzzyness that affected the last post)


;; Test Cases
(svg-file "windmill" (make-windmill [1 1 1]))

(svg-file "windmill" (make-windmill [1 1 2]))
(svg-file "windmill" (make-windmill [1 2 1]))
(svg-file "windmill" (make-windmill [3 0 0]))
(svg-file "windmill" (make-windmill [3 1 3]))
(svg-file "windmill" (make-windmill [3 3 1]))

(hjoin '() '()) ; ()
(hjoin (make-windmill [1 1 1]) '()) ; ([0 0 "red"] [0 1 "white"] [0 -1 "white"] [-1 0 "green"] [1 0 "green"]) ; ()
(hjoin (make-windmill [1 1 1])(make-windmill [1 1 1])) ; ([0 0 "red"] [0 1 "white"] [0 -1 "white"] [-1 0 "green"] [1 0 "green"] [4 0 "red"] [4 1 "white"] [4 -1 "white"] [3 0 "green"] [5 0 "green"])

(svg-file "windmill" (hjoin (make-windmill [1 1 1]) (make-windmill [1 2 1])))

(svg-file "windmill" (reduce hjoin '() (list
                                        (make-windmill [1 1 1])
                                        (make-windmill [1 2 1])
                                        (make-windmill [1 3 1]))))


(svg-file "windmill" (hcombine
                      (make-windmill [1 1 1])
                      (make-windmill [1 2 1])
                      (make-windmill [1 3 1])))

(svg-file "windmill" (hcombine))


(svg-file "windmill")
(svg-file "windmill" '())
(svg-file "windmill" (make-composite-rectangle 0 0 1 1 "white" ) (make-windmill [1 1 1]) '() (make-windmill [1 1 1]) )

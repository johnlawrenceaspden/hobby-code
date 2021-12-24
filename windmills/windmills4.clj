#!/usr/bin/env clojure

;; Fermats' Christmas Theorem: Some Early Orbits

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of code from previous posts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Let's start with the most simple candidate number, 5 = 4.1 + 1

;; We can always find a thin cross for any number of form 4n+1, almost by definition
(defn make-thin-cross [n] [1 1 (/ (- n 1) 4)])

(make-thin-cross 5) ; [1 1 1] 

;; [1 1 1] is in fact the only triple that can represent 5, can you see why?

(svg-file "windmills4-1" (make-windmill [1 1 1]))

;; This triple is a fixed point of the red transform (crosses always are)
(red   [1 1 1]) ; [1 1 1]

;; It's also a fixed point of the green transform (because n=p)
(green [1 1 1]) ; [1 1 1]

;; It's a fixed point of the green transform because its arms are squares, so I'll call it a square-bladed windmill

;; If we have a square-bladed windmill then we can transform it into an odd and an even square
(defn victory [[s n p]]
  (assert (= n p))
  (hjoin
   (make-composite-rectangle 0 0 s s "red")
   (concat
    (make-composite-rectangle 0 0 n n "green")
    (make-composite-rectangle n n n n "green")
    (make-composite-rectangle 0 n n n "white")
    (make-composite-rectangle n 0 n n "white"))))

(svg-file "windmills4-2" (victory [1 1 1]))

;; We see that 5 = 1*1 + 2*2


;; What about the next candidate number up, 9 = 4.2+1

(make-thin-cross 9) ; [1 1 2]

;; again, a red fixed point (because it's a cross)

(red [1 1 2]) ; [1 1 2]

;; But not a green one

(green [1 1 2]) ; [1 2 1]

;; however that is a fixed point of the red transform, so that's as far as we can go
(red [1 2 1]) ; [1 2 1]

;; If we look at this 'orbit' of the triple [1 1 2] as windmills, we see:

(apply svg-file "windmills4-3" (map make-windmill '([1 1 2] [1 2 1])))

;; We see a thin cross, a red fixed point, connected to a square, also a red fixed point

;; We've got two red fixed points connected by the green transform.

;; The square tells us that 9 has a factor, it's not a prime number


;; Let's do the next one

(make-thin-cross 13) ; [1 1 3]

(red [1 1 3]) ; [1 1 3]  ;; crosses are red fixed points

(green [1 1 3]) ; [1 3 1]

(red [1 3 1]) ; [3 1 1]

(green [3 1 1]) ; [3 1 1]

;; We've found a green fixed point, a square-bladed windmill

;; Our orbit is:
(apply svg-file "windmills4-4" (map make-windmill '([1 1 3] [1 3 1][3 1 1])))

;; We've got a red fixed point, connected by a green step and then a red step to a green fixed point

(svg-file "windmills4-5" (victory [3 1 1]))

;; The green fixed point gives us our decomposition into odd and even squares

;; 13 = 3*3 + 2*2

;; The Christmas theorem is looking good so far....

(make-thin-cross 17) ; [1 1 4] ;; cross

(green [1 1 4]) ; [1 4 1]

(red [1 4 1]) ; [3 1 2]

(green [3 1 2]) ; [3 2 1]

(red [3 2 1]) ; [1 2 2]

(green [1 2 2]) ; [1 2 2]  ;; square blades!

;; Our orbit is:
(apply svg-file "windmills4-6" (map make-windmill '([1 1 4] [1 4 1][3 1 2] [3 2 1] [1 2 2])))

;; A red fixed point connected to a green fixed point by four steps. green,red,green,red

(svg-file "windmills4-7" (victory [1 2 2]))

;; 17 = 1*1 + 4*4



(make-thin-cross 21) ; [1 1 5]

(green [1 1 5]) ; [1 5 1]

(red [1 5 1]) ; [3 1 3]

(green [3 1 3]) ; [3 3 1]

(red [3 3 1]) ; [3 3 1]

;; Our orbit is:
(apply svg-file "windmills4-8" (map make-windmill '([1 1 5] [1 5 1][3 1 3] [3 3 1])))

;; A red fixed point connected to a red fixed point by three steps. green,red,green

;; The red fixed point is a cross. Because the red square is not 1x1, I'm going to call that a 'fat cross'

;; If we have a fat cross, that tells us that our number has a factorization

;; Look:

(svg-file "windmills4-9"
       (concat
        (make-composite-rectangle 0 0 3 3 "red")
        (make-composite-rectangle 3 0 1 3 "green")
        (make-composite-rectangle 4 0 1 3 "white")
        (make-composite-rectangle 5 0 1 3 "green")
        (make-composite-rectangle 6 0 1 3 "white")))


;; In fact, it tells us that 21 = 3*(3+4) = 3*7

;; The pattern here is that our initial thin crosses (red fixed points) are always connected to other fixed points

;; If the other fixed point is green, it shows us that we have an odd and even square that represent our number

;; If the other fixed point is red, then it shows us that our number has a factor, i.e that it isn't prime.

;; That's the Christmas Theorem.

;; Why would it be true???



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of blog post
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note, to convert the svg files to png for posting on blogger, can do:
;; for i in windmills3*svg; do rsvg-convert -z 2 "$i" -o "${i%svg}png"; done

;; -z 2 (zoom factor 2 prevents the fuzzyness that affected the last post)


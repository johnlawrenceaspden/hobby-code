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

(svg-file "windmill" (make-windmill [1 1 1]))

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

(svg-file "windmill" (victory [1 1 1]))

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

(apply svg-file "windmill" (map make-windmill '([1 1 2] [1 2 1])))

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
(apply svg-file "windmill" (map make-windmill '([1 1 3] [1 3 1][3 1 1])))

;; We've got a red fixed point, connected by a green step and then a red step to a green fixed point

(svg-file "windmill" (victory [3 1 1]))

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
(apply svg-file "windmill" (map make-windmill '([1 1 4] [1 4 1][3 1 2] [3 2 1] [1 2 2])))

;; A red fixed point connected to a green fixed point by four steps. green,red,green,red

(svg-file "windmill" (victory [1 2 2]))

;; 17 = 1*1 + 4*4



(make-thin-cross 21) ; [1 1 5]

(green [1 1 5]) ; [1 5 1]

(red [1 5 1]) ; [3 1 3]

(green [3 1 3]) ; [3 3 1]

(red [3 3 1]) ; [3 3 1]

;; Our orbit is:
(apply svg-file "windmill" (map make-windmill '([1 1 5] [1 5 1][3 1 3] [3 3 1])))

;; A red fixed point connected to a red fixed point by three steps. green,red,green

;; The red fixed point is a cross. Because the red square is not 1x1, I'm going to call that a 'fat cross'

;; If we have a fat cross, that tells us that our number has a factorization

;; Look:

(svg-file "windmill"
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















































(apply svg-file "windmill" (map make-windmill '([1 1 1])))


case, [1 1 1] representing 5=4.1+1

;; [1 1 1] is in fact the only triple that can represent 5




;; So it's a fixed point of both the red and the green transforms
;; and it's orbit is just '([1 1 1])



;; It's a fixed point of the red transform because it's a cross
;; Because it's arms and square are only one wide, I'm going to call it a 'thin cross'.

;; We can always find a thin cross for any number of form 4n+1, almost by definition


(make-thin-cross 5) ; [1 1 1]



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

(svg-file "windmill" (victory [1 1 1]))


;; Let's look at the next candidate number, 9
























;; Let's take a triple at random

[7 3 8]

;; It so happens that this triple represents 145

(total [7 3 8]) ; 145

(svg-file "windmill" (make-windmill [7 3 8]))

;; The red transform connects it to another triple
(red [7 3 8]) ; [1 12 3]

;; And so does the green
(green [7 3 8]) ; [7 8 3]

;; so our triple is not a fixed point of either transform.

;; And there are some things to notice here.

;; Firstly, the red and green transforms connect it to two different triples.

;; That's got to be true, because the red transform always changes the size of the red square (unless it's a fixed point)
;; And the green transform never does.

;; And we should also notice that the green transform is self-inverse, i.e., if we do it again, we get our original triple back
;; That's obvious, because all the green transform does is to swap n and p
(green (green [7 3 8])) ; [7 3 8]

;; But more interestingly, the red transform is self-inverse too
(red (red [7 3 8])) ; [7 3 8]

;; It has to be, because of the way we constructed it.

;; There's only one thing to do (at most!), and you can always reverse it.
;; Think about this, play with it, until you're sure it's true.

(svg-file "windmill" (make-windmill (green (green [7 3 8]))) (make-windmill (green [7 3 8]))(make-windmill [7 3 8])(make-windmill (red [7 3 8]))(make-windmill (red (red [7 3 8]))))

;; That means that there's never any point to doing the either transform twice.

;; In order to make progress, to explore the space of triples, we need to do red, then green, then red, then green, .....
;; In this process, we'll find that every triple is associated with a sequence of triples, which we'll call its orbit

;; Heres part of the orbit of [7 3 8]
(svg-file "windmill"
          (make-windmill [7 3 8])
          (make-windmill (red [7 3 8]))
          (make-windmill (green (red [7 3 8])))
          (make-windmill (red (green (red [7 3 8]))))
          (make-windmill (green (red (green (red [7 3 8])))))
          (make-windmill (red (green (red (green (red [7 3 8])))))))



;; So a natural question is, can this process go on forever, always finding new triples?

;; The answer to that question is an easy no, because there are only so many triples for each number
































;; We can make up triples where s=n=p at will, they're always fixed points of both red and green, so they're never connected to anything else

(let [[s n p] [3 3 3]]
  (svg-file "windmill"  (make-windmill [s n p]) (victory [s n p])))

(total [3 3 3]) ; 45

(let [[s n p] [5 5 5]]
  (svg-file "windmill"  (make-windmill [s n p]) (victory [s n p])))

(total [5 5 5]) ; 125

;; So we know that numbers that are like 5, 45, 125 can be expressed as sums of odd and even squares.
   






;; So let's write an iterator to describe the orbit of a triple

;; We'll look at the green then red then green then red case.... We could also go the other way, red first then green then...

(defn iteraterg [triple orbit]
  (let [rtriple (red triple)
        rgtriple (green rtriple)]
    [ rgtriple (cons rtriple (cons triple orbit))]))



(iteraterg [1 1 1] '())


(cons rtriple (cons triple orbit))










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of blog post
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note, to convert the svg files to png for posting on blogger, can do:
;; for i in windmills3*svg; do rsvg-convert -z 2 "$i" -o "${i%svg}png"; done

;; -z 2 (zoom factor 2 prevents the fuzzyness that affected the last post)


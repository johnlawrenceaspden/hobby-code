#!/usr/bin/env clojure

;; Fermats' Christmas Theorem: Automatic Windmills

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


;; So far so good, but the red transform is a bit manual for my taste. It's easy to see how to do it
;; by eye, but I want to automate that.

;; Let's go over our sequence of shapes for 37 again
(svg-file "windmill" (make-windmill [1 1 9]) (make-windmill [1 9 1]) (make-windmill [3 1 7])(make-windmill [3 7 1])(make-windmill [5 1 3])(make-windmill [5 3 1])(make-windmill [1 3 3]))

;; The red transform has a way of flipping the shapes and exchanging the white and green colours, which I'm going to ignore.

;; There may be a more principled way of drawing the triples so that the associated windmills are
;; more consistent with each other, but I don't want to get distracted. I'm already deep down one
;; rabbit hole, and so I'm going to avoid this branch tunnel, or at least save it for later.

;; So, automating the red transform:

;; By eye it's always obvious what to do, but it's not so obvious to a computer,

;; There are actually six different cases to consider, depending on the relative sizes of s, n and p!

;; Let's fix s to be seven, and n to be three, and let p vary, that should be enough to capture all the different cases

;; Start off with p as one, the smallest it can be

;; The picture for the transformation we want to do is:
(svg-file "windmill" (make-windmill [7 1 3]) (make-windmill [5 9 1]))

;; We see that the arms of the windmill are all extending into the square, taking squares until they
;; collide with the other arms.

;; Because the arms stay n wide throughout, the collision happens when the arms have all extended by s-n squares.

;; So the normal length 3 gets 6 (7-1) added to it, to become 9, the parallel length 1 stays the same

;; And the red square loses the parallel length taken off it on both sides, so s 7 becomes s 7 - 2*1

;; We might think that the transformation would take [7 1 3] to [5 1 9]

;; Or in general terms, s -> s-2p n->n+(s-p), p->p

(svg-file "windmill" (make-windmill [7 1 3]) (make-windmill [5 1 9]))

;; But no! During the transformation, the arm that started at the north-west corner extended down so that it's now the arm that
;; starts at the south-west corner, so we need to make our triple with 1 as the normal length and 9 as the parallel length

;; That's what causes the red-green colour flip in our drawings

;; So in fact what we want is s -> s-2p p->n+(s-p), n->p

(svg-file "windmill" (make-windmill [7 1 3]) (make-windmill [5 9 1]))

;; In order for this procedure to work, we need s-2p to be positive, or equivalently p < s/2

;; In code:
(defn red [[s p n]]
  (cond (< p (/ s 2)) [(- s (* 2 p)) (+ n (- s p)) p]
        :else [s p n]))

;; If the condition's not true, we'll just hand back the original triple (for now...)

(red [7 1 3]) ; [5 9 1]

(let [[s p n][7 1 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; So now let's increase p by one
(let [[s p n][7 2 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; Looks good, and again
(let [[s p n][7 3 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; but increasing p again
(let [[s p n][7 4 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; we see that our idea has failed, that transformation would take up more squares than we have (p >  s/2)
;; So we've just given up and returned the original triple

;; Notice that we haven't seen the case where p=s/2, because s is odd, so s/2 isn't an integer

;; If p> s/2, we can do roughly the same thing, but we have to stop earlier.

;; If we imagine our arms extending in, then we'll see that they have to stop after three moves, when they collide with each other
;; that three comes from s-p, so the normal length needs to extend by s-p, and the square needs to lose s-p from both sides
;; And in this case, we don't need to swap n and p, the normal length remains the normal length.

;; The condition for being able to do this is that s-p is positive, or s>p

;; And the transformation is s->s-2(s-p), n->n+(s-p), p->p

;; Adding this case to our transform
(defn red [[s p n]]
  (cond (< p (/ s 2))   [(- s (* 2 p))       (+ n (- s p)) p]
        (< (/ s 2) p s) [(- s (* 2 (- s p))) p (+ n (- s p))]
        :else [s p n]))

(red [7 4 3]) ; [1 4 6]

(let [[s p n][7 4 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; In this case, our drawing appears to flip, because the corners of the red square have moved!
;; But we can see that the shape is the same, and the number of squares is the same.

;; keep increasing p
(let [[s p n][7 5 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; again it works

;; keep increasing p
(let [[s p n][7 6 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; and again
(let [[s p n][7 7 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; this time p=s, and so our procedure does nothing.

;; In fact, there's nothing we can do here, we can neither shrink nor expand the red square while keeping the shape the same.

;; I'm going to call this special, more symmetric type of windmill a 'cross'. It's a fixed point of our red
;; transform, we just let it return the same triple.

;; What if we make p even bigger?
(let [[s p n][7 8 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; Neither of our previous ideas will work, but there's a new option here, we can make the square bigger at the expense of the arms
(svg-file "windmill" (make-windmill [7 8 3]) (make-windmill [9 8 2]))

;; Again, the shape appears to flip, but it's just a mirror image of the shape we would get if we extended the red square

;; so how do we code this case?

;; Well, the red square can get bigger (on both sides), by the amount by which the parallel length exceeds the square
;; i.e. p-s
;; The parallel length stays the same, and the normal length goes down by p-s

;; so s-> s+2*(p-s), p->p, n-> n-(p-s)
;; And this should work as long as p-s<n, or p<n+s



(defn red [[s p n]]
  (cond (< p (/ s 2))   [(- s (* 2 p))       (+ n (- s p)) p]
        (< (/ s 2) p s) [(- s (* 2 (- s p))) p (+ n (- s p))]
        (< s p (+ n s)) [(+ s (* 2 (- p s))) p (- n (- p s))]
        :else [s p n]))

(red [7 8 3]) ; [9 8 2]

(let [[s p n][7 8 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; increase p
(let [[s p n][7 9 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; looks good

;; increase p
(let [[s p n][7 10 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; Now p=s+n, and we can't make any of our previous ideas work.

;; We could increase the red square to fill the whole shape now, but the result wouldn't be a windmill, and there's nothing
;; else we can do, so we'll leave this case as a fixed point
































;; from my drawing
;; case 1 : (p < s/2)     decrease, delta is -p, negative, add to n, swap : s->s-2p, n->n+s-p , swap p and n
;; case 2 : (s/2 < p < s) decrease, delta is (p-s) negative, add to n , don't swap
;; case 4 : (s < p < s+n) increase, delta is (p-s) positive, take p-s from n , don't swap
;; case 3 : (p > s+n), increase, delta is n, take it from p, swap





















;; And how do we choose delta??




;; So let's go through them one by one

;; Let's fix s to be 7, and n to be 3, and vary p

;; When p is small, we can shrink the red square by extending the blades normally into the square

(svg-file "windmill" (make-windmill [7 1 3]) (make-windmill [5 9 1]))

;; How much do we shrink the square? By p on every side, so for this to work, 2p must be less than s
;; If it won't work, let's just give the triple back unmodified because we can't work out what to do

;; notice also that the normal length has now become the parallel length in the drawing!

(defn red [[s p n]]
  (cond (< (* 2 p) s) (let [delta (- p)]
                        (red-normal-swap [s p n] delta))
        :else [s p n]))

(red [7 1 3]) ; [5 9 1]

(let [[s p n][7 1 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n])) (make-windmill (red (red [s p n])))))

;; increasing p
(let [[s p n][7 2 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; increasing p
(let [[s p n][7 3 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; increasing p so it's too big to do this
(let [[s p n][7 4 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))

;; so what should we do in this case, where 2p>s?
;; well, we do the same thing, but we can't extend the blades in as far, delta must be p - s
;; this should be ok as long as p isn't actually bigger than s
(defn red [[s p n]]
  (cond (< (* 2 p) s) (let [delta (- p)]
                        (red-normal-swap [s p n] delta))
        (< s (* 2 p) (* 2 s) )(let [delta (- p s)]
                        (red-normal [s p n] delta))
        :else [s p n]))


(let [[s p n][7 4 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n]))))
;; looks good, you can see that the shapes are the same, even though there's a quarter-turn rotation

;; so increasing p more
(let [[s p n][7 5 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n])) (make-windmill (red (red [s p n])))))
;; still good

(let [[s p n][7 6 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n])) (make-windmill (red (red [s p n])))))

;; now p = s 
(let [[s p n][7 7 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n])) (make-windmill (red (red [s p n])))))

;; and we can't do anything, so doing nothing is correct

;; increasing p
(let [[s p n][7 8 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n])) (make-windmill (red (red [s p n])))))

;; in this case, where p is bigger than s, but not as big as p+n
;; we should add p-s to each side of the square, and take it off the normal
(defn red [[s p n]]
  (cond (< (* 2 p) s) (let [delta (- p)]
                        (red-normal-swap [s p n] delta))
        (< s (* 2 p) (* 2 s) )(let [delta (- p s)]
                                (red-normal [s p n] delta))
        (< s p (+ n s)) (let [delta (- p s)]
                                (red-normal [s p n] delta))
        :else [s p n]))

(let [[s p n][7 8 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n])) (make-windmill (red (red [s p n])))))

;; again, looking good

;; increase p some more
(let [[s p n][7 9 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n])) (make-windmill (red (red [s p n])))))

;; and again
(let [[s p n][7 10 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n])) (make-windmill (red (red [s p n])))))

;; again, there's nothing sensible we can do here, if we increase the red square by three, then we don't have a windmill any more

;; increase p 
(let [[s p n][7 11 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n])) (make-windmill (red (red [s p n])))))

;; finally when p is bigger than s+n,
;; can increase the size of the square by n, and that comes off the parallel length, and again we need to swap normal and parallel to keep the shape

(defn red [[s p n]]
  (cond (< (* 2 p) s) (let [delta (- p)]
                        (red-normal-swap [s p n] delta))
        (< s (* 2 p) (* 2 s) )(let [delta (- p s)]
                                (red-normal [s p n] delta))
        (< s p (+ n s)) (let [delta (- p s)]
                          (red-normal [s p n] delta))
        (< (+ n s) p)   (let [delta n]
                          (red-parallel-swap [s p n] delta))
        :else [s p n]))

(let [[s p n][7 11 3]] (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red [s p n])) (make-windmill (red (red [s p n])))))







[7 5 3] ; [7 5 3]
(red [7 5 3]) ; [3 5 5]

[7 6 3]
(red [7 6 3]) ; [5 4 6]

(red (red [7 6 3])) ; [3 7 4]

(total [7 6 3])





























;; And how big should delta be?

;; Well, when we're increasing the red square, we can do that by the normal length

;; And when we're decreasing it, we decrease by the size of the red square less the parallel length


17=1+4.4



(svg-file "windmill" (make-windmill [1 1 9]) (make-windmill [1 9 1]) (make-windmill [3 1 7])(make-windmill [3 7 1])(make-windmill [5 1 3])(make-windmill [5 3 1])(make-windmill [1 3 3]))

25 = 6*4 +1 

(svg-file "windmill" (make-windmill [1 1 6]) (make-windmill [1 6 1]) (make-windmill [3 1 4]) (make-windmill [3 4 1]) (make-windmill [5 0 0]))

33 = 1 + 4.8

(svg-file "windmill" (make-windmill [1 1 8]) (make-windmill [1 8 1]) (make-windmill [3 6 1]) (make-windmill [5 1 2]) (make-windmill [5 2 1]) (make-windmill [1 4 2]) (make-windmill [1 2 4]) (make-windmill [3 2 3]) (make-windmill [3 3 2]))





;; Let's look at our red transform again, it actually works differently depending on whether we're
;; increasing or reducing the size of the red square

;; If we're increasing the size of the square, the squares needed

























;; I'm worried about the shape flipping that went on in the last post, so let's see if we can fix that

(square, parallel, normal)



[1 1 9]
(green [1 1 9]) ; [1 9 1]
(red-parallel [1 9 1] 1) ; [3 1 7]
(green [3 1 7]) ; [3 7 1]
(red-parallel [3 7 1] 1) ; [5 1 3]
(green [5 1 3]) ; [5 3 1]
(red-normal [5 3 1] -2) ; [1 3 3]
[1 3 3]





















;; In this case
(svg-file "windmill" (make-windmill [3 1 7]))
;; we want to pull the red square in one place, and give resulting 8 spare squares to the arms
;; lengthening the normal arm length

;; The new shape should be:
(svg-file "windmill" (make-windmill [1 9 1])) ; nil

;; So we need to lengthen the normal, and then swap normal and parallel to keep the shape the same
(defn red-normal [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm p)]
    [news (+ n lengthchange) p ]))


(svg-file "windmill" (make-windmill [3 1 7])) ; nil ; nil
(red-normal [3 1 7] -1) ; [1 9 1] ; [1 9 1]

(svg-file "windmill" (make-windmill [1 9 1])) ; nil ; nil

;; But in this case, if we increase the size of the red square, we want to reduce the parallel length
;; and then swap the normal and parallel

(svg-file "windmill" (make-windmill [1 9 1])) ; nil ; nil
(svg-file "windmill" (make-windmill [3 1 7])) ; nil ; nil




;; Our red transform doesn't do this
(red [1 9 1] +1) ; [3 7/9 9]

;; It looks like we need a different red transform for this case

(red-parallel [1 9 1] +1) ; [3 1 7]
(red-normal   [1 9 1] +1) ; [3 7/9 9]
(red-parallel [3 1 7] -1) ; [1 7 9/7]
(red-normal   [3 1 7] -1) ; [1 9 1]





(svg-file "windmill" ) ; nil















(red [1 9 1] +1) ; [3 7/9 9] ; [3 7/9 9]

(svg-file "windmill" (make-windmill [1 9 1])) ; nil






































;; So, let's take another example of a number which is of form 4n+1

;; 25 = 4 * 6 + 1

(svg-file "windmill" (make-windmill [1 1 6]))

(defn total [[s p n]]
  (+ (* s s) (* 4 p n)))

(total [1 1 6]) ; 25

;; We have code for our red and green transforms

(defn red [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm p)]
    [news (+ n lengthchange) p ]))

(defn green [[s p n]] [s n p])


(total [1 1 9]) ; 37 ; 37 ; 
[1 1 9] ; [1 1 9] ; [1 1 9] ; [1 1 9]
(svg-file "windmill" (make-windmill [1 1 9])) ; nil ; nil ; nil
(red [1 1 9] 0) ; [1 1 9] ; [1 1 9] ; [1 1 9] ; [1 9 1]
(svg-file "windmill" (make-windmill [1 1 9])) ; nil ; nil ; nil
(green [1 1 9]) ; [1 9 1] ; [1 9 1]
(svg-file "windmill" (make-windmill [1 9 1])) ; nil ; nil ; nil ; nil
(red [1 9 1] 1) ; [3 9 7/9] ; [3 9 7/9]
(svg-file "windmill" (make-windmill [3 1 7])) ; nil ; nil ; nil
(red [3 1 7] -1) ; [1 1 9]
(total [1 1 9]) ; 37
(svg-file "windmill" (make-windmill [1 1 9])) 


(svg-file "windmill" (make-windmill [3 1 7]))
[3 1 7] -1
news 1
spare 9-1=8
sparesperarm 2 
lengthchange 


(svg-file "windmill" (make-windmill [1 9 1])) ; nil ; nil ; nil ; nil
;; urrgh
(red [1 9 1] 1) ; [3 9 7/9] ; [3 9 7/9]






























[1 1 6]
(total [1 1 6]) ; 25
(svg-file "windmill" (make-windmill [1 1 6]))

;; We can't change the size of the red square here, while still keeping the same shape
;; That's because the size of the red square is the same as the (parallel) width of the arms, which we've called p

;; What should we do here?

;; I'm tempted to throw an error, saying that the red transform is impossible, but instead,
;; I'm going to say the the red transform should do nothing, i.e. this triple is a fixed point

;; We want to be able to calculate delta automatically, so this first case should look like

(defn delta [[s p n]]
  (cond (= s p) 0))

(delta [1 1 6]) ; 0

(red [1 1 6] 0) ; [1 1 6]

;; We can just say:
(let [t [1 1 6]] (red t (delta t))) ; [1 1 6]

;; OK, the red transform doesn't help, let's try the green one

(green [1 1 6]) ; [1 6 1]
(svg-file "windmill" (make-windmill [1 6 1])) ; nil

;; That looks good, we can increase the size of the red square, delta should be one, because we can
;; expand the red square by exactly the normal width of the arms (n)

(defn delta [[s p n]]
  (cond (= s p) 0)
  :else n)

(delta [1 6 1]) ; 1

(red [1 6 1] 1) ; [3 17/3 1] ; [3 6 2/3]

(let [t [1 6 1]] (red t (delta t))) ; [3 6 2/3]




























(red [1 1 6] 1) ; [3 1 4]
(total [3 1 4]) ; 25
(svg-file "windmill" (make-windmill [3 1 4]))
(green [3 1 4]) ; [3 4 1]
(total [3 4 1]) ; 25
(svg-file "windmill" (make-windmill [3 4 1]))
(red [3 4 1] 1) ; [5 4 0] ;
(total [5 4 0]) ; 25
(svg-file "windmill" (make-windmill [5 4 0]))

;; I don't like what's happened here!

;; The red transform has moved us out of our space of windmills, and just left a red square
























;; Let's do another example in a more principled way

;; We'll think in terms of triples [s, p, n]

;; Where s is the size of the red square, p (parallel) is the width of the arms , and n (normal) is
;; the length of the arms.

;; Here's a function to draw the windmill that represents such a triple



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


(defn red-transform [[s p n]]
  (cond (p < (/ s 2)) (red-normal-swap [s p n] (- p))
        ((/ s 2) < p < s) (red-normal  [s p n] (- p s))
        (s < p < (+ s n)) (red-normal  [s p n] (- p s))
        (p > (+ s n))     (red-parallel-swap [s p n] n)
        :else              [s p n]))
                                       
  



(defn involution [[s p n]]
  (cond (< s (- p n))         [( + s (* 2 n)) n (- p s n)]       ;; delta is n , alter p and swap (my case 3)
        (< (- p n) s (* 2 p)) [( - (* 2 p) s) p (+ (- s p) n)]   ;; delta is -p, alter n          
        (> s (* 2 p))         [( - s (* 2 p)) (+ (- s p) n) p])) ;; delta is -p, alter n and swap (my case 1)



(defn involution [[s p n]]
  (cond (< s (- p n))         [( + s (* 2 n)) n (- p s n)]       ;; delta is n , alter p and swap (my case 3)
        (< (- p n) s (* 2 p)) [( - (* 2 p) s) p (+ (- s p) n)]   ;; delta is -p, alter n          
        (> s (* 2 p))         [( - s (* 2 p)) (+ (- s p) n) p])) ;; delta is -p, alter n and swap (my case 1)

(let [old [1 2 5]]
  (let [new (involution old)]
    (println old new)
    (svg-file "windmill"  (make-windmill old) (make-windmill new) )))


(< 3 7)
;; first case
(involution   [3 8 1]) ; [5 1 4]        ; [5 1 4]
(red-parallel-swap [3 8 1] 1) ; [5 1 4] ; [5 1 4]

(< 5 -3)
(< (-3) 5 8)
;; middle case
   
(involution       [5 1 4]) ; [3 8 1]    ; [3 8 1]
(red-normal-swap  [5 1 4] -1) ; [3 8 1] ; [3 8 1]

(involution   [3 4 2]) ; [5 4 1]
(red-normal   [3 4 2] 1) ; [5 4 1]

(involution   [5 4 1]) ; [3 4 2] ; [3 4 2]
(red-normal   [5 4 1] -1) ; [3 4 2] ; [3 4 2]


(defn t [[s p n] transform delta]
  (let [[ns np nn :as new] (transform [s p n] delta)]
    (assert (> ns 0))
    (assert (> np 0))
    (assert (> nn 0))
    (println [s p n] "->" new )
    (svg-file "windmill" (make-windmill [s p n]) (make-windmill new))))

(red-parallel [1 3 1] 1) ; [3 1 1]

(t [1 1 1] red-parallel 1)
(t [1 2 1] red-parallel 1)
(t [1 3 1] red-parallel 1)
(t [1 5 1] red-parallel-swap 1)
(t [1 7 1] red-parallel-swap 1)
(t [1 8 1] red-parallel-swap 1)

case 1 : (p < s/2)     decrease, delta is -p,   negative, add to n, swap
case 2 : (s/2 < p < s) decrease, delta is (p-s) negative, add to n , don't swap
case 4 : (s < p < s+n) increase, delta is (p-s) positive, take p-s from n , don't swap
case 3 : (p > s+n), increase, delta is n, take it from p, swap









(defn red-parallel-swap [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm n)]
    [news n ( + p lengthchange)]))

(defn red-parallel [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm n)]
    [news ( + p lengthchange) n]))


(defn red-normal [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm p)]
    [news p (+ n lengthchange) ]))

(defn red-normal-swap [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm p)]
    [news (+ n lengthchange) p ]))



;; p > s + n 
(let [[s p n] [1 5 1] delta 1]
  (println [s p n] (red-parallel-swap [s p n] delta))
  (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red-parallel-swap [s p n] delta))))

(let [[s p n] [1 7 1] delta 1]
  (println [s p n] (red-parallel-swap [s p n] delta))
  (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red-parallel-swap [s p n] delta))))

(let [[s p n] [3 7 1] delta 1]
  (println [s p n] (red-parallel [s p n] delta))
  (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red-parallel [s p n] delta))))

(let [[s p n] [5 7 1] delta 1]
  (println [s p n] (red-parallel [s p n] delta))
  (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red-parallel [s p n] delta))))

(let [[s p n] [5 7 2] delta 2]
  (println [s p n] (red-parallel [s p n] delta))
  (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red-parallel [s p n] delta))))

(let [[s p n] [5 7 3] delta 2]
  (println [s p n] (red-parallel [s p n] delta))
  (svg-file "windmill" (make-windmill [s p n]) (make-windmill (red-parallel [s p n] delta))))




(svg-file "windmill" (make-windmill [1 9 1]) (make-windmill [3 7 1]))

;; And sometimes we need to add the squares to the normal length (or remove them)


(red-normal [5 3 1] -2) ; [1 3 3]

(svg-file "windmill" (make-windmill [5 3 1])(make-windmill [1 3 3]))


;; Notice that the red transform can actually work in four different ways

;; Sometimes to keep the same shape, we need to add the spare squares to the parallel length of the blades, (or remove them)
(defn red-parallel-swap [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm n)]
    [news n ( + p lengthchange)]))

(defn red-parallel [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm n)]
    [news ( + p lengthchange) n]))


(defn red-normal [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm p)]
    [news p (+ n lengthchange) ]))

(defn red-normal-swap [[s p n] delta]
  (let [news  (+ s (* 2 delta))
        spare (- ( * s s ) (* news news))
        sparesperarm (/ spare 4)
        lengthchange (/ sparesperarm p)]
    [news (+ n lengthchange) p ]))



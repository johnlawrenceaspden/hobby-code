;; Feynman's Arrows II : OK, so what are the complex numbers?
;; requires [simple-plotter "0.1.2"] 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here's some code from the previous post

;; use pomegranate to get the library, if it's not already on your classpath
(require 'cemerick.pomegranate) 
(cemerick.pomegranate/add-dependencies 
 :coordinates '[[simple-plotter "0.1.2"]] 
 :repositories {"clojars" "http://clojars.org/repo"})

(use 'simple-plotter)

;; Make blackboards to draw arrows on
(defn make-blackboard [title size]
  (create-window title 400 400 white black (- size) size (- size) size)
  (axes)
  (ink yellow))

;; Draw an arrow shape from (a,b) to (c,d)
(defn draw-offset-arrow [[a b][c d]]
  (let [headx (+ a c) 
        heady (+ b d)]
    (line a b headx heady)
    (line headx heady (+ headx (* -0.086 c) (* -0.05 d)) (+ heady (*  0.05 c) (* -0.086 d)))
    (line headx heady (+ headx (* -0.086 c) (*  0.05 d)) (+ heady (* -0.05 c) (* -0.086 d)))))

;; Draw one of our arrows, which always have their tails at 0
(defn draw-arrow [[a b]] (draw-offset-arrow [0 0] [a b]))

;; Here's everything we know about the arrows so far:
(defn add-arrows[[a b][c d]]   
  [(+ a c) (+ b d)])

(defn multiply-arrows[[a b][c d]]
  [(- (* a c) (* d b)) (+ (* a d) (* c b))])

;; That's easier to read in the standard prefix notation
;; (a,b)+(c,d) -> (a+b, c+d)
;; (a,b)*(c,d) -> (ac-db, ad+cb)

;; The addition rule says: treat arrows as if they represented vector displacements, and add them nose to tail
;; The multiplication rule says: treat arrows as if they were zooms and rotations, and use one to zoom and rotate the other.

;; We also had a few favourite arrows that we'd played with:
(def arrow1 [3,4])
(def arrow2 [4,-3])
(def arrow3 [1,1/10])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(do (make-blackboard "Favourite Arrows" 6)
    (doseq [i [arrow1 arrow2 arrow3]] (draw-arrow i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The first thing I want you to notice is that there's a subset of
;; the arrows that works exactly like the numbers that we all got
;; comfortable with in primary school. They're the ones where the 
;; arrows point due east or west.

(add-arrows [10 0] [5 0]) ;-> [15 0]
;; (10,0) + (5,0) -> (15,0) is like 10 + 5 -> 15

(multiply-arrows [4 0] [3 0]) ;-> [12 0]
;; (4,0) * (3,0) -> (12,0) is like 4 * 3 -> 12

(multiply-arrows [-1 0] [-1 0]) ;-> [1 0]
;; (-1,0) * (-1,0) -> (1,0) is like   -1 * -1 -> 1 

;; You probably learned that as an arbitrary rule, but it's obvious if
;; you think of -1 as meaning 'leave it the same size but reverse it'


;; And so since it doesn't matter whether we think about (3,0) or 3,
;; we'll just forget about the difference, and sometimes write (3,0),
;; and sometimes write 3, depending on convenience.

;; We say that the 'real numbers' are 'embedded' in the arrows. What we
;; mean is that there's a structure in the arrows that's just like the
;; real numbers, and so wherever we were going to use real numbers we
;; can just use horizontal arrows instead and everything will work out
;; exactly the same.

;; There's another subset of the arrows, that point due north and due south.

;; Under addition, they're just like the real numbers too.

;; (0, 10)+ (0, 3) -> (0, 13)

;; But under multiplication, they end up turning each other into horizontal arrows

;; (0, 10) * (0, 4) -> (0x0-10x4, 0x10+4x0) = (-40, 0)

;; You can see why pretty easily. 

;; (0, 10) represents 'turn 90 degrees clockwise and magnify by 10',
;; and (0, 4) means 'turn 90 degrees clockwise and magnify by 4'

;; And the product (-40, 0) means turn 180 degrees and multiply by 40. 

;; In this view, (-1, 0), or just -1 means 'turn 180 degrees (no zooming!)'

;; And the (0,1)*(0,1) -> (-1, 0) is just the fact 'if you turn 90
;; degrees clockwise and don't zoom, and then you turn another 90
;; degrees clockwise and don't zoom, then that's the same as if you'd
;; turned 180 degrees, without zooming'.

;; So as long as we're talking about arrows, the thing we've called
;; -1, or the pair (-1,0), or the arrow length 1 that points east, or
;; the idea of turning through 180 degrees, does have a 'square root'.

;; There is a thing, the arrow length 1 that points straight north, or
;; the pair (0,1), or the idea of turning 90 degrees, that if you
;; multiply it by itself you get -1.

;; That's important, but it's also trivial.

;; Two quarter-turns clockwise make a 180 turn.

(multiply-arrows [0,1] [0,1]) ;-> [-1 0]

;; Two quarter-turns make a half-turn
(do (make-blackboard "Something Whose Square is (-1,0)" 2)
    (draw-arrow [0,1])
    (ink red)
    (draw-arrow (multiply-arrows [0,1] [0,1]))))

;; We call that upwards pointing length 1 arrow i, for historical reasons. 
(do (make-blackboard "The mysterious and magical i" 2)
    (draw-arrow [0,1])))

;; If 1 is another name for (1,0), and i is the name we've given to (0,1), then notice that
;; 5+3i is (5,0)+(3,0)*(0,1) -> (5,3)

;; So we have yet another way of describing our pairs

(defn print-arrow [[a b]]
   (str "the pair (" a "," b "), "
        "also known as the complex number " a "+"b"i, "
        "also known as the arrow " (cond (> a 0) (str a " north") (= a 0) "" :else (str (- a) "south"))
        " and " (cond (> b 0) (str b " east") (= b 0) "" :else (str (- b) "west"))))

(print-arrow arrow1) ;-> "the pair (3,4), also known as the complex number 3+4i, also known as the arrow 3 north and 4 east"
(print-arrow [1/11 0.25]) ;-> "the pair (1/11,0.25), also known as the complex number 1/11+0.25i, also known as the arrow 1/11 north and 0.25 east"

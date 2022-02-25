(require 'cemerick.pomegranate)
(cemerick.pomegranate/add-dependencies 
 :coordinates '[[simple-plotter "0.1.2"]] 
 :repositories {"clojars" "http://clojars.org/repo"})

(use 'simple-plotter.core)

(defn make-blackboard [title size]
  (create-window title 400 400 white black (- size) size (- size) size)
  (axes)
  (ink yellow))


(make-blackboard "yo" 6)

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

(add-arrows [10 0] [5 0])

(do (make-blackboard "Adding positive real numbers" 16)
    (draw-arrow [5 0])
    (draw-arrow [10 0])
    (ink red)
    (draw-arrow (add-arrows [10 0] [5 0])))

(do (make-blackboard "Wessel^h^h^h^h^h^h Argand Diagram" 10)
    (dotimes [i 100] (draw-arrow [(- (rand-int 20) 10) (- (rand-int 20) 10)])))

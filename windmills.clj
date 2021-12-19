#!/usr/bin/env clojure

;; Fermats' Christmas Theorem: Windmills

;; Sorry for the delay, had COVID, I'm fine, don't worry!

;; Let's pick an arbitrary number of the form 4n+1, say 29

;; Precisely because it's of form 4n+1, we can split it into a central square and four identical
;; blocks, in this case, a 1x1 square and four 1x7 blocks

;; 29 = 1*1 + 4 * (1 * 7)
(defn square [n] (* n n))

(+ (square 1) (* 4 (* 1 7))) ; 29

;; Lets draw that:

;; I'm not going to explain how the svg making thing works, but see:
;; http://www.learningclojure.com/2010/10/generating-xml-to-make-svg-vector.html
;; if you're curious.

(require 'clojure.xml)

(def squaresize 10)

(defn make-rect [i j colour]
  {:tag :rect
   :attrs {:x (str (* i squaresize))
           :y (str (* j squaresize))
           :width  (str squaresize)
           :height (str squaresize)
           :style (str "fill:", colour, ";stroke:black;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1")}})

(defn make-svg [objects]
  {:tag :svg
   :attrs {:width "100%"
           :height "100%"
           :version "1.1"
           :xmlns "http://www.w3.org/2000/svg"}
   :content (for [[i j c] (adjust-list objects)] (make-rect i j c))})


(defn svg-file [filename objects]
  (spit (str filename ".svg") (with-out-str (clojure.xml/emit (make-svg objects)))))

(svg-file "windmill" (list [0 0 "red"]))

(svg-file "windmill" 
          (list [0 0 "red"]
                [1 0 "red"]
                [2 0 "red"]))

(svg-file "windmill"
          (list [0 0 "green"]
                [0 1 "green"]
                [0 2 "green"]))



(def cross (list [0 0 "red"]
                [1 0 "white"]
                [2 0 "white"]
                [0 1 "green"]
                [0 2 "green"]
                [-1 0 "white"]
                [-2 0 "white"]
                [0 -1 "green"]
                [0 -2 "green"]))












(defn adjust-list [rectlist]
  (let [hmin (apply min (map first  rectlist))
        vmin (apply min (map second rectlist))]
    (for [[a b c] rectlist] [(- a hmin) (- b vmin) c]))) 
        
(adjust-list cross)

(svg-file "windmill" 
           (list [0 0 "red"]
                 [1 0 "white"]
                 [2 0 "white"]
                 [0 1 "green"]
                 [0 2 "green"]
                 [-1 0 "white"]
                 [-2 0 "white"]
                 [0 -1 "green"]
                 [0 -2 "green"]))




(defn orange [n]
  (if (< n 0)
    (range 0 n -1)
    (range 0 n 1)))

(defn make-composite-rectangle [h v hsquares vsquares colour]
  (for [i (orange hsquares) j (orange vsquares)] [(+ i h) (+ j v) colour]))

(make-composite-rectangle 0 0 3 1 "red")
(make-composite-rectangle 0 0 -3 -1 "red")

(svg-file "windmill" 
          (concat (list [0 0 "red"])
                  (make-composite-rectangle 1 0   2 2 "white")
                  (make-composite-rectangle 0 1   2 2 "green")
                  (make-composite-rectangle -1 0 -2 -2 "white")
                  (make-composite-rectangle 0 -1 -2 -2 "green")))








(svg-file "windmill" (list (make-composite-rectangle 0 0 3 1 "red")))



(spit "windmill.svg" (with-out-str (clojure.xml/emit (make-svg (list (make-composite-rectangle 20 30 4 5 "red"))))))






;; The other day, I wanted to make some graph paper.

;; While experimenting with inkscape, I noticed that svg is actually an xml file format.

;; Since clojure is good with xml, that means that it's actually easier to make such a drawing with a program:

;; Each element of the drawing is represented as a map


;; The whole file is represented as a map containing those maps




(defn make-svg [gridsize squaresize]
  {:tag :svg
   :attrs {:width "100%"
           :height "100%"
           :version "1.1"
           :xmlns "http://www.w3.org/2000/svg"}
   :content (concat #_(make-composite-rectangle 20 30 3 3 "green")
                    (make-composite-rectangle 20 30 4 5 "red") )})

;; The library clojure.contrib.lazy-xml will turn the nested map into xml:

(require 'clojure.xml)

;; We can use with-out-str to capture the output, which is unaccountably printed
;; rather than given back as a string, and spit to write it to a file.

(spit "windmill.svg" (with-out-str (clojure.xml/emit (make-svg 10 80))))

;; The nice thing about this is that you can then use inkscape to modify the
;; file, and then diff to work out how to take the modifications back into the
;; program. Does anybody know how to make the emit function format the xml so
;; that the output file is nicely readable?

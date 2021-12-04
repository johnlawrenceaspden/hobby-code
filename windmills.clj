#!/usr/bin/env clojure


;; 25 = 1*1 + 4*1.6

;; The other day, I wanted to make some graph paper.

;; While experimenting with inkscape, I noticed that svg is actually an xml file format.

;; Since clojure is good with xml, that means that it's actually easier to make such a drawing with a program:

;; Each element of the drawing is represented as a map

(defn make-rect [i j squaresize colour]
  {:tag :rect
   :attrs {:x (str i)
           :y (str j)
           :width  (str squaresize)
           :height (str squaresize)
           :style (str "fill:", colour, ";stroke:black;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1")}})

;; The whole file is represented as a map containing those maps

(defn make-composite-rectangle [v h vsquares hsquares colour]
  (for [i (range hsquares) j (range vsquares)] (make-rect (+ i v) (+ j h) 10 colour)))



(defn make-svg [gridsize squaresize]
  {:tag :svg
   :attrs {:width "100%"
           :height "100%"
           :version "1.1"
           :xmlns "http://www.w3.org/2000/svg"}
   :content (concat (make-composite-rectangle 20 30 3 3 "green")
                    (make-composite-rectangle 80 90 4 5 "red") )})

;; The library clojure.contrib.lazy-xml will turn the nested map into xml:

(require 'clojure.xml)

;; We can use with-out-str to capture the output, which is unaccountably printed
;; rather than given back as a string, and spit to write it to a file.

(spit "windmill.svg" (with-out-str (clojure.xml/emit (make-svg 10 80))))

;; The nice thing about this is that you can then use inkscape to modify the
;; file, and then diff to work out how to take the modifications back into the
;; program. Does anybody know how to make the emit function format the xml so
;; that the output file is nicely readable?

(ns simple-plotter
  (:import (javax.swing JFrame JPanel )
           (java.awt Color Graphics Graphics2D Image))
  (:use (clojure.contrib def )))

;; This is an attempt to make graphics in clojure as simple as it was on a ZX
;; spectrum. Let us count the whole maven/leiningen/cake-clojure-emacs-(require
;; 'simple-plotter) thing as being a one-time effort equivalent to persuading
;; one's father to buy a ZX spectrum in the first place.

;; Define some colours to use:
;; Call java.awt.Color/GREEN simple-plotter/green

(defmacro- defcolours [& colours]
  (list* 'do (map #(list 'def  (symbol (. (str %) toLowerCase)) (symbol (str "Color/" (str %)))) colours)))

(defcolours black blue cyan darkGray gray green lightGray magenta orange pink red white yellow)

;; Private machinery

(defvar- lines        (atom []))
(defvar- inkcolor     (atom green))
(defvar- papercolor   (atom black))
(defvar- current-position (atom [0,0]))
(defvar- defaultink   (atom green))
(defvar- defaultpaper (atom black))
(declare thepanel)


(defn- draw-lines [#^Graphics2D g2d ]
  (doseq [[x1 y1 x2 y2 color] @lines]
    (. g2d setColor color)
    (. g2d drawLine x1 y1 x2 y2)
    (. g2d drawLine x1 y1 x2 y2)))

(defn- render [ #^Graphics2D g w h ]
  (doto g
    (.setColor @papercolor)
    (.fillRect 0 0 w h))
    (draw-lines g))

(defn- create-panel []
    "Create a panel with a customised render"
  (proxy [JPanel] []
    (paintComponent [g]
		    (proxy-super paintComponent g)
		    (render g (. this getWidth) (. this getHeight)))))

(defvar- thepanel (create-panel))

(defn- primitive-repaint []
  (. thepanel repaint))

(defn- primitive-line [x1 y1 x2 y2]
  (swap! lines conj [x1 y1 x2 y2 @inkcolor])
  (primitive-repaint))

(defn- set-current-position [x1 y1]
  (swap! current-position (constantly [x1 y1])))

(defn- set-ink [color]
  (swap! inkcolor (constantly color)))

(defn- set-paper [color]
  (swap! papercolor (constantly color))
  (primitive-repaint))

(defn- set-default-ink [color]
  (swap! defaultink (constantly color)))

(defn- set-default-paper [color]
  (swap! defaultpaper (constantly color)))

(defn- remove-lines[] (swap! lines (constantly [])))

(defn- reset[]
  (set-ink @defaultink)
  (set-paper @defaultpaper)
  (set-current-position 0 0)
  (remove-lines)
  (primitive-repaint))

(defn- make-scalars [points xleft xright ytop ybottom]
  (let [xmax (reduce max (map first points))
        xmin (reduce min (map first points))
        ymax (reduce max (map second points))
        ymin (reduce min (map second points))]
    [(fn[x] (+ xleft (* (/ (- x xmin) (- xmax xmin))    (- xright xleft))))
     (fn[y] (+ ybottom  (* (/ (- y ymin) (- ymax ymin)) (- ytop ybottom))))]))

;; Public Interface

(defn create-window
  ([] (create-window "Simple Plotter"))
  ([title] (create-window title 1024 768))
  ([title width height] (create-window title width height white black ))
  ([title width height ink paper]
     (set-ink ink)
     (set-paper paper)
     (let [frame (JFrame. title)]
       (doto frame
         (.add thepanel)
                                        ; the extra space 2,32 is taken up by window decoration
                                        ; how to get that from the OS? 
         (.setSize (+ width 2) (+ height 32))
         (.setVisible true)))))

(defn cls[]
  (remove-lines)
  (primitive-repaint))

(defn plot [x1 y1]
  (primitive-line x1 y1 x1 y1)
  (set-current-position x1 y1))

(defn draw [dx dy]
  (let [[x1 y1] @current-position
        [x2 y2] [(+ x1 dx) (+ y1 dy)]]
    (primitive-line x1 y1 x2 y2)
    (set-current-position x2 y2)))

(defn line [x1 y1 x2 y2]
  (plot x1 y1)
  (draw (- x2 x1) (- y2 y1)))

(defn ink [color] (set-ink color))

(defn paper [color] (set-paper color))

(defn scaled-scatter-plot [points xleft xright ytop ybottom scalepoints]
  (let [[xsc ysc] (make-scalars (take scalepoints points) xleft xright ytop ybottom)]
      (doseq [[x y] points]
        (plot (* (xsc x))
              (* (ysc y))))))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples

(defn sine-example[]

  (use 'simple-plotter)
  (create-window)

  (cls)
  
  ;; sine graph
  (doseq [x (range 1024)]
    (plot x (+ 384 (* 376 (Math/sin (* Math/PI (/ x 512)))))))

  ;; axes
  (ink yellow)
  (plot 0 384) (draw 1024 0)
  (line 512 0 512 1024))


;;(sine-example)













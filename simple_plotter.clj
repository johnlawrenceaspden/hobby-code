(ns simple-plotter
  (:import (javax.swing JFrame JPanel )
           (java.awt Color Graphics Graphics2D Image))
  (:use (clojure.contrib def )))

(defmacro- defcolours [& colours]
  (list* 'do (map #(list 'def  (symbol (. (str %) toLowerCase)) (symbol (str "Color/" (str %)))) colours)))

(defcolours black blue cyan darkGray gray green lightGray magenta orange pink red white yellow)

(defvar- lines (atom []))
(defvar- inkcolor (atom green))
(defvar- papercolor (atom black))
(defvar- current-position (atom [0,0]))

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

(defn create-window
  ([] (create-window "Simple Plotter"))
  ([title] (create-window title 1024 768))
  ([title width height]
  (let [frame (JFrame. title)]
    (doto frame
      (.add thepanel)
      (.setSize (+ width 2) (+ height 32)) ; the extra space is taken up by window decoration
                                        ; how to get that from the OS? 
      (.setVisible true)))))

(defn cls[]
  (swap! lines (constantly []))
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

(defn ink [color]
  (swap! inkcolor (constantly color)))

(defn paper [color]
  (swap! papercolor (constantly color))
  (primitive-repaint))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example

;; (use 'simple-plotter)

;; (create-window "ZX Graphics" 256 176)

;; ;; 5 CLS

;; (cls)

;; ;; 10 FOR X = 1 TO 255
;; ;; 20 PLOT X, 88+80*SIN(X/128*PI)
;; ;; 30 NEXT X

;; (doseq [x (range 256)]
;;   (plot x (+ 88 (* 80 (Math/sin (* Math/PI (/ x 128)))))))

;; ;; Hey, let's have some axes as well

;; ;; 40 INK 6
;; ;; 50 PLOT 0,88: DRAW 255,0
;; ;; 60 PLOT 127,0: DRAW 0,168

;; (ink yellow)
;; (plot 0 88) (draw 255 0)
;; (plot 127 0) (draw 0 168)

;; ;; And a border

;; (ink red)
;; (plot 0 0) (draw 255 0) (draw 0 168) (draw -255 0) (draw 0 -168)

;; ;; And a blue line right through everything

;; (ink blue)
;; (line 0 0 255 168)
















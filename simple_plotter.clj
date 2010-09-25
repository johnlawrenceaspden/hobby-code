(ns simple-plotter
  (:import (javax.swing JFrame JPanel )
           (java.awt Color Graphics Graphics2D Image))
  (:use (clojure.contrib def)))

;; This is an attempt to make graphics in clojure as simple as it was on a ZX
;; Spectrum. Let us count the whole maven/leiningen/cake-clojure-emacs-(require
;; 'simple-plotter) thing as being a one-time effort equivalent to persuading
;; one's father to buy a ZX Spectrum in the first place.

;; Define some colours to use:
;; java.awt.Color/GREEN -> simple-plotter/green

(defmacro- defcolours [& colours]
  (list* 'do (map #(list 'def  (symbol (. (str %) toLowerCase)) (symbol (str "Color/" (str %)))) colours)))

(defcolours black blue cyan darkGray gray green lightGray magenta orange pink red white yellow)

;; Private machinery

(defn- draw-lines [lines #^Graphics2D g2d xs ys]
  (doseq [[x1 y1 x2 y2 color] @lines]
    (. g2d setColor color)
    (. g2d drawLine (* xs x1) (* ys y1) (* xs x2) (* ys y2))))

(defn- render-lines-to-graphics [lines paper-color height width
                                 #^Graphics2D g w h ]
  (doto g
    (.setColor @paper-color)
    (.fillRect 0 0 w h))
    (draw-lines lines g (/ w @width) (/ h @height)))

(defn- primitive-repaint [plotter]
  (. (plotter :panel) repaint))

(defn create-plotter [title width height ink paper]
  (let [lines       (atom [])
        height      (atom height)
        width       (atom width)
        paper-color (atom paper)
        panel       (proxy [JPanel] []
                      (paintComponent [g]
                                      (proxy-super paintComponent g)
                                      (render-lines-to-graphics
                                       lines paper-color height width
                                       #^Graphics2D g
                                       (. this getWidth)
                                       (. this getHeight))))
        frame (JFrame. title)]
    (doto frame
      (.add panel)
      ;; The extra space 2,32 is taken up by the window decorations
      ;; in GNOME. How to get that from the OS? 
      (.setSize (+ @width 2) (+ @height 32))
      (.setVisible true))
    {:height height
     :width  width
     :ink-color (atom ink)
     :paper-color paper-color
     :current-position (atom [0,0])
     :lines lines
     :panel panel}))


(defn- primitive-line [plotter x1 y1 x2 y2]
  (let [ink @(:ink-color plotter)]
    (swap! (:lines plotter) conj [x1 y1 x2 y2 ink]))
  (primitive-repaint plotter))

(defn- set-paper-color [plotter color]
  (swap! (plotter :paper-color) (constantly color))
  (primitive-repaint plotter))

(defn- set-ink-color [plotter color]
  (swap! (plotter :ink-color) (constantly color)))

(defn- set-current-position [plotter [x y]]
  (swap! (plotter :current-position) (constantly [x y])))

(defn- remove-lines [plotter] (swap! (plotter :lines) (constantly [])))

(defn- make-scalars [points xleft xright ytop ybottom]
  (let [xmax (reduce max (map first points))
        xmin (reduce min (map first points))
        ymax (reduce max (map second points))
        ymin (reduce min (map second points))]
    [(fn[x] (+ xleft (* (/ (- x xmin) (- xmax xmin))    (- xright xleft))))
     (fn[y] (+ ybottom  (* (/ (- y ymin) (- ymax ymin)) (- ytop ybottom))))]))


(defvar- current-plotter (atom nil))

;; ;; Public Interface

(defn create-window
  ([] (create-window "Simple Plotter"))
  ([title] (create-window title 1024 768))
  ([title width height] (create-window title width height white black ))
  ([title width height ink paper]
     (let [plotter (create-plotter title width height ink paper)]
       (swap! current-plotter (constantly plotter))
       plotter)))

;;Makes a version of a function with an implicit first argument of plotter, and
;;a default version with no first argument is not supplied, and which uses
;;current-plotter instead.
(defmacro ddefn [fnname args & body]
  `(defn ~fnname
     (~args (~fnname @~'current-plotter ~@args))
     ([~'plotter ~@args] ~@body)))

;; So that instead of saying
;; (defn plot
;;   ([x1 y1] (plot @current-plotter x1 y1))
;;   ([plotter x1 y1]
;;      (primitive-line plotter x1 y1 x1 y1)
;;      (set-current-position plotter [x1 y1])))
;; we can say:

(ddefn plot [x1 y1]
       (primitive-line plotter x1 y1 x1 y1)
       (set-current-position plotter [x1 y1]))

(ddefn cls [] 
       (remove-lines plotter)
       (primitive-repaint plotter))

(ddefn plot [x1 y1]
     (primitive-line plotter x1 y1 x1 y1)
     (set-current-position plotter [x1 y1]))

(ddefn draw [dx dy]
  (let [[x1 y1] @(plotter :current-position)
        [x2 y2] [(+ x1 dx) (+ y1 dy)]]
    (primitive-line plotter x1 y1 x2 y2)
    (set-current-position plotter [x2 y2])))

(ddefn draw-to [x2 y2]
  (let [[x1 y1] @(plotter :current-position)]
    (primitive-line plotter x1 y1 x2 y2)
    (set-current-position plotter [x2 y2])))

(ddefn line [x1 y1 x2 y2]
   (plot plotter x1 y1)
   (draw plotter (- x2 x1) (- y2 y1)))

(ddefn ink   [color] (set-ink-color plotter color))

(ddefn paper [color] (set-paper-color plotter color))

(ddefn scaled-scatter-plot [points xleft xright ytop ybottom scalepoints]
  (let [[xsc ysc] (make-scalars (take scalepoints points) xleft xright ytop ybottom)]
      (doseq [[x y] points]
        (plot (* (xsc x))
              (* (ysc y))))))

(defn window [plotter]
  (swap! current-plotter (fn[x] plotter)))

(ddefn get-height [] @(plotter :height))

(ddefn get-width  [] @(plotter :width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples

(defn sine-example[]

  (create-window "sine")

  (cls)
  
  ;; sine graph
  (doseq [x (range 1024)]
    (plot x (+ 384 (* 376 (Math/sin (* Math/PI (/ x 512)))))))

  ;; axes
  (ink yellow)
  (plot 0 384) (draw 1024 0)
  (line 512 0 512 1024))

;;(sine-example)

;; (defn examples []
;;   (sine-example)
;;   (in-ns 'user)
;;   (use 'simple-plotter)
;;   (load-file "fractal-fern.clj")
;;   (load-file "zxsin.clj")
;;   (load-file "gridpattern.clj") )

;;(examples)













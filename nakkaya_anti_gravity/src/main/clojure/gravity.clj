(ns gravity.core
  (:refer-clojure :exclude [+ - * =])
  (:use (clojure.contrib.generic [arithmetic :only [+ - *]]
				 [comparison :only [=]]))
  (:use [vector-2d.core]))

(defn gravity-vector [u v]
  (let [force (/ 2000 (Math/pow (magnitude (- u v)) 2))
	angle (bearing u v)] 
    (vector-2d (* (Math/sin angle) force) (* (Math/cos angle) force))))

(defn total-gravitational-force [state]
  (apply + (map #(gravity-vector (:player @state) %) (:obstacles @state))))

(defn seek [state]
  (normalize (+ (normalize (- (:target @state) (:player @state)))
		(total-gravitational-force state))))

(defn steer [state]
  (when (> (dist (:player @state) (:target @state)) 1)
    (dosync (alter state assoc :player (+ (:player @state) (seek state))))))

(defn circle [g pt rad color]
  (let [[x y] (vals pt)
	offset (int (/ rad 2))
	x (- x offset)
	y (- y offset)]
    (doto g
      (.setColor color)
      (.fill (java.awt.geom.Ellipse2D$Double. x y rad rad)))))

(defn board [state]
  (proxy [javax.swing.JPanel
	  java.awt.event.ActionListener] []
    (paintComponent
     [g]
     (.setColor g java.awt.Color/WHITE)
     (.fillRect g 0 0 (.getWidth this) (.getHeight this))
     (circle g (:player @state) 20 java.awt.Color/GREEN)
     (doseq [pt (:obstacles @state)] 
       (circle g pt 60 java.awt.Color/BLUE)))
    (actionPerformed 
     [e]
     (steer state)
     (.repaint this))))

(defn mouse-listener [state]
  (let [dragging (ref nil)]
    (proxy [java.awt.event.MouseAdapter 
	    java.awt.event.MouseMotionListener] [] 
      (mousePressed 
       [e]
       (dosync (alter state assoc :target 
		      (vector-2d (.getX e) (.getY e))))))))

(defn frame []
  (let [state (ref {:player (vector-2d 325 200) :target (vector-2d 325 200)
		    :obstacles [(vector-2d 379 90)
				(vector-2d 135 223) (vector-2d 560 290)
				(vector-2d 550 120) (vector-2d 145 70)
				(vector-2d 350 320) (vector-2d 60 50)
				(vector-2d 210 313) (vector-2d 450 210)]})
	board (board state)
	timer  (javax.swing.Timer. 5 board)
	mouse-listener (mouse-listener state)]
    (.addMouseListener board mouse-listener)
    (.start timer)
    (doto (javax.swing.JFrame.)
      (.setAlwaysOnTop true)
      (.add board)
      (.setSize 650 400)
      (.setVisible true))))

(comment
  (frame)
  )

(ns drawing-demo
  (:import [javax.swing JPanel JFrame]
           [java.awt Dimension]))
 

(defn render [g w h]
  (.drawLine g 0 0 w h)
  (.drawLine g 0 h w 0)
  (.drawRect g (* 1/4 w) (* 1/4 h) (* 1/2 w) (* 1/2 h))
  (.drawOval g (* 1/4 w) (* 1/4 h) (* 1/2 w) (* 1/2 h))
  ;(.drawString g "the centre" (* 1/2 w) (* 1/2 h))
)
     

(def panel
     (proxy [JPanel] []  
	     (paintComponent [g] 
			     (proxy-super paintComponent g)
			     (render g (. this getWidth) (. this getHeight)))))

(. panel setPreferredSize (Dimension. 200 200))

(def frame (JFrame. "A window with a cross in it"))
(. frame add panel)
(. frame pack)
(. frame show)

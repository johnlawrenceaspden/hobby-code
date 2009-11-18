; -- API START --
(defn radians [degrees] (. java.lang.Math toRadians degrees))
(defmacro rot [g2d angle & body]
  `(do (. ~g2d rotate (radians ~angle))
    (do ~@body)
    (. ~g2d rotate (radians (- 0 ~angle)))))
(defmacro trans [g2d dx dy & body]
  `(do (. ~g2d translate ~dx ~dy)
    (do ~@body)
    (. ~g2d translate (- 0 ~dx) (- 0 ~dy))))
; -- API END --


; -- CLIENT CODE START --
(def width 400)
(def height 400)

(defn draw_tree [g2d length depth]
  (if (> depth 0)
    (do
      (. g2d drawLine 0 0 length 0)
      (trans g2d (int length) 0
        (rot g2d -30
          (draw_tree g2d (* length 0.75) (- depth 1)))
        (rot g2d 30
          (draw_tree g2d (* length 0.75) (- depth 1)))
      )
    )))
(defn draw [g2d]
  (draw_tree g2d 50 10))
; -- CLIENT CODE END --


; -- API START --  
(def frame (new javax.swing.JFrame))
(def display (proxy [javax.swing.JPanel] []
  (paintComponent [g2d]
    (. g2d translate (/ width 2) (/ height 2))
    (. g2d rotate (radians -90))
    (draw g2d)
  )
))
;(.setDefaultCloseOperation frame
;  javax.swing.WindowConstants/EXIT_ON_CLOSE)
(.setContentPane frame display)
(.pack frame)
(.setSize frame width height)
(.show frame)
; -- API END --
(import '(javax.swing JFrame JPanel )
        '(java.awt Color Graphics Graphics2D Image)
        '(java.awt.image BufferedImage))

(def lines (atom []))

(def bufferedimage
     (new BufferedImage 640 400 BufferedImage/TYPE_INT_ARGB))

(def graphics (. bufferedimage createGraphics))

(defn draw-lines [ #^Graphics g2d ]
  (doseq [[x1 y1 x2 y2 color] @lines]
    (. g2d setColor color)
    (. g2d drawLine x1 y1 x2 y2)
    (. g2d drawLine x1 y1 x2 y2)))

(defn render [ #^Graphics g w h ]
  (doto g
    (.setColor (Color/BLACK))
    (.fillRect 0 0 w h))
    (draw-lines g))

(defn create-panel []
    "Create a panel with a customised render"
  (proxy [JPanel] []
    (paintComponent [g]
		    (proxy-super paintComponent g)
		    (render g (. this getWidth) (. this getHeight)))))

(def thepanel (create-panel))

(defn create-window [title]
  (let [frame (JFrame. title)]
    (doto frame
      (.add thepanel)
      (.setSize 640 400)
      (.setVisible true))))

(defn plotline [x1 y1 x2 y2 c]
  (swap! lines conj [x1 y1 x2 y2 c])
  (. thepanel repaint))

(defn plot [x1 y1 c]
  (plotline x1 y1 x1 y1 c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(create-window "Title")
(plotline (rand 640)(rand 400)(rand 640)(rand 400) Color/GREEN)
(plot (rand 640) (rand 400) Color/YELLOW) 














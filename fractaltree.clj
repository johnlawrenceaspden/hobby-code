(import '(javax.swing JFrame JPanel )
	'(java.awt Color Graphics Graphics2D))

(defn ^:static draw-tree [ #^Graphics g2d ^double angle ^double x ^double y ^double length ^double branch-angle ^long depth]
  (if (> depth 0)
    (let [new-x (- x (* length (Math/sin (Math/toRadians angle))))
	  new-y (- y (* length (Math/cos (Math/toRadians angle))))
	  new-length (fn [] (* length (+ 0.75 (rand 0.1))))
	  new-angle  (fn [op] (op angle (* branch-angle (+ 0.75 (rand)))))]
      (. g2d drawLine x y new-x new-y)
      (#'draw-tree g2d (new-angle +) new-x new-y (new-length) branch-angle (- depth 1))
      (#'draw-tree g2d (new-angle -) new-x new-y (new-length) branch-angle (- depth 1)))))

(defn render [ #^Graphics g w h ]
  (doto g
    (.setColor (Color/BLACK))
    (.fillRect 0 0 w h)
    (.setColor (Color/GREEN)))
  (let [init-length ( / (min w h) 5),
	branch-angle (* 10 (/ w h)),
	max-depth 12]
    (draw-tree  g 0.0 (/ w 2) h init-length branch-angle max-depth)))

(defn create-panel []
    "Create a panel with a customised render"
  (proxy [JPanel] []
    (paintComponent [g]
		    (proxy-super paintComponent g)
		    (time (render g (. this getWidth) (. this getHeight))))))


(defn run []
  (let [frame (JFrame. "Clojure Fractal Tree")
	panel (create-panel)]
    (doto frame
      (.add panel)
      (.setSize 640 400)
      (.setVisible true))))

(run)
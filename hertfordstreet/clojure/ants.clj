(import
 '(java.awt Color Graphics Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))


(def dim 80)    
(def nants-sqrt 2)          ;number of ants = nants-sqrt^2

(def animation-sleep-ms 100)
(def ant-sleep-ms 40)

(def running true)

(def white (. (new Color 255 255 255 255) (getRGB)))
(def black (. (new Color 0 0 0 255) (getRGB)))

(defstruct cell :color)


(def board    
     (apply vector
            (map (fn [_]
                   (apply vector
                          (map (fn [_] (ref (struct cell 0)))
                                      (range dim))))
                 (range dim))))


(defn coordinate [[x y]]
  (-> board (nth x) (nth y)))

(defstruct ant :dir)

(defn spawn-ant
  [loc dir]
    (dosync
      (let [p (coordinate loc)
            a (struct ant dir)]
        (alter p assoc :ant a)
        (agent loc))))


;;; GET USERINPUT, SPAWN 1 - 4 ANTS
(defn setup
  []
  (let [start-range '(35 36 37 38 39 40 41 42 43 44 45)]
    (dosync
     (doall
      (for [x start-range]
        (do
          (spawn-ant [x 40] (rand-int 3)))))
     (doall
      (for [x (range dim) y (range dim)]
        (if (= 0 (rem x 2))
          (do (alter (coordinate[x y]) assoc :color white))
	  (do (alter (coordinate[x y]) assoc :color black))
	  ))))))


(def dir-delta {0 [0 -1]
                1 [1 0]
                2 [0 1]
                3 [-1 0]})
                
                

(defn next-loc
  [[x y] dir]
    (let [[dx dy]
          (dir-delta dir)]
      [(+ x dx) (+ y dy)]))

(defn turn
  "turns the ant at the location by the given amount"
  [loc amt]
    (dosync
     (let [p (coordinate loc)
           ant (:ant @p)]
       (alter p assoc :ant (assoc ant :dir (+ (:dir ant) amt)))))
    loc)

(defn move
  "moves the ant in the direction it is heading. Must be called in a
  transaction that has verified the way is clear"
  [loc]
     (let [oldp (coordinate loc)
           ant (:ant @oldp)
           newloc (next-loc loc (:dir ant))
           p (coordinate newloc)]
         ;move the ant
       (alter p assoc :ant ant)
       (alter oldp dissoc :ant)))


(defn behave
  "the main function for the ant agent"
  [loc]
  (let [p (coordinate loc)
        ant (:ant @p)
        ahead (coordinate (next-loc loc (:dir ant)))
        ahead-left (coordinate (next-loc loc (dec (:dir ant))))
        ahead-right (coordinate (next-loc loc (inc (:dir ant))))
        places [ahead ahead-left ahead-right]]
    (dosync
     (when running
       (send-off *agent* #'behave))
     (. Thread (sleep ant-sleep-ms))
     (move loc))))
     
       ;foraging
;       (cond
;        (and (pos? (:food @p)) (not (:home @p)))
;          (-> loc take-food (turn 4))
;        (and (pos? (:food @ahead)) (not (:home @ahead)) (not (:ant @ahead)))
;          (move loc)
;        :else
;          (let [ranks (merge-with +
;                                  (rank-by (comp :food deref) places)
;                                  (rank-by (comp :pher deref) places))]
;          (([move #(turn % -1) #(turn % 1)]
;            (wrand [(if (:ant @ahead) 0 (ranks ahead))
;                    (ranks ahead-left) (ranks ahead-right)]))
;           loc)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;pixels per world cell
(def scale 10)

;;; .
(defn fill-cell [#^Graphics g x y c]
  (doto g
    (setColor c)
    (fillRect (* x scale) (* y scale) scale scale)
    (drawLine 1 1 10 10)))


(comment
(defn render-ant [ant #^Graphics g x y]
  (let [black (. (new Color 0 0 0 255) (getRGB))
        gray (. (new Color 100 100 100 255) (getRGB))
        red (. (new Color 255 0 0 255) (getRGB))
        white (. (new Color 255 255 255 255) (getRGB))]        
    (doto g
      (setColor (if (:color ant)
                  black white))
      (. fillRect x y (+ x 5) (+ y 5))))))
      


(defn render-place [g p x y]
  (fill-cell g x y (. (new Color 255 100 25 255) (getRGB))))

(defn render [g]
  (let [v (dosync
           (apply vector
                  (for [x (range dim) y (range dim)]
                       @(coordinate [x y]))))
        
        img (new BufferedImage (* scale dim) (* scale dim)
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (setColor (. Color white)))
    (. g (drawImage img 0 0 nil))
    (dorun
     (for [x (range dim) y (range dim)]
       (render-place bg (v (+ (* x dim) y)) x y)))
    
    (. bg (dispose))))

(def panel (doto (proxy [JPanel] []
                        (paint [g] (render g)))
             (setPreferredSize (new Dimension
                                    (* scale dim)
                                    (* scale dim)))))

(def frame (doto (new JFrame) (add panel) (pack) (show)))

(def animator (agent nil))

(defn animation [x]
  (when running
    (send-off *agent* #'animation))
  (. panel (repaint))
  (. Thread (sleep animation-sleep-ms))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;demo
;(load-file "/Users/rich/dev/clojure/ants.clj")
;(def ants (setup))
;(send-off animator animation)
;(dorun (map #(send-off % behave) ants))

;(load-file "/home/jacksmack/Coding/Coding/Clojure/ants.clj")

(defn go
    (def ants (setup))
  (send-off animator animation)
  (dorun (map #(send-off % behave) ants)))
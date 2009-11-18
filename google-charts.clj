;;(in-ns 'google-chart)
;;(clojure/refer 'clojure)

(def *amp* "&")
 
; ---- connect to google charts and retrieve a chart ----
(defn get-google-chart [request]
  (new javax.swing.ImageIcon (new java.net.URL request) "Google Chart"))
 
; ---- pop-up a png in a dialog frame ----
(import '(javax.swing JLabel JFrame))
 
(defn show-chart [request]
  (println "Showing chart: " request)
  (doto (new JFrame "Google Chart")
    (.add (new JLabel (get-google-chart request)))
    (.pack)
    (.setVisible true)))
 
; display the hello-world chart
;(show-chart (str "http://chart.apis.google.com/chart?"
;                 "cht=p3&chd=s:hW&chs=250×100&chl=Hello|World"))
 
; ---- simple encoding to 0 - 61 in chars ----
 
(defn encode [val scale]
  (if (nil? val)
      "_"
      (let [val (. Math (floor (* 61 (/ val scale))))]
        (nth "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" val))))
 
; ---- build a chart ----
 
(defn base-url []
  "http://chart.apis.google.com/chart?")
 
(defn chart-size [x y]
  (str "chs=" x "x" y))
 
(defn chart-type [type]
  (str "cht=" type))
 
(defn chart-data [data]
  (let [mx (apply max data)]
    (apply str "chd=s:" (map #(encode % mx) data))))   ; function literal

;### fix spaces -&gt; +
(defn chart-title [title]
  (str "chtt=" title))

(defn chart-axis [x y]
  ; really basic, just an x and y axis
  (apply str "chxt=x,y" *amp*
         "chxl=0:" (concat (interleave (repeat "|") x)
                           ["|1:"] (interleave (repeat "|") y))))

;; Test it out ...
(comment
(show-chart
  (str
    (base-url)
    (chart-size 480 480) *amp*
    (chart-type "lc") *amp*
    (chart-data (map (fn [x] (* x x)) (range -50 51))) *amp*
    (chart-title "x+squared") *amp*
    (chart-axis ["-5" "0" "5"] ["" "" "y-axis"]))))
 
; Test it out ...
(show-chart
  (str
    (base-url)
    (chart-size 480 240) *amp*
    (chart-type "lc") *amp*
    (chart-data [188.5 186 181.5 183 179.5 181]) *amp*
    (chart-title "Jonathan+Weight") *amp*
    (chart-axis ["Week+1" "Week+2" "Week+3"] ["" "" "190+lbs"])))

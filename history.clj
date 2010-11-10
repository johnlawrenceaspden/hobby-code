(use ['clojure.contrib.lazy-xml :as 'lazy-xml])

(defn xstr [x] (str (* 80 x)))
(defn ystr [y] (str (* 80 (inc y))))
(defn yhighstr[y] (str (- (* 80 (inc y)) 40)))


(defn year->xy [year]
  (let [year (- year 1)
        lastdigit (mod year 10)
        decade (/ (- (mod year 100) (mod year 10)) 10)]
    (if (even? decade) [ lastdigit (- 9 decade)]
        [(- 9 lastdigit) (- 9 decade)])))

(= (map year->xy '(1900 1901 1909 1910 1911 1920 1921)) '([0 0] [1 0] [9 0] [9 1] [8 1] [0 2] [1 2]))



(defn make-rect [[x y] style]
  {:tag :rect
   :attrs {:x (xstr x)
           :y (xstr y)
           :width "80"
           :height "80"
           :style style}})

(defn make-coloured-rect [[x y] colour]
  (make-rect [x y] (str "fill:"colour";stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1")))


(defn make-red-rect [[x y]]
  (make-coloured-rect [x y] "#ff0000"))

(defn make-white-rect [[x y]]
  (make-coloured-rect [x y] "#ffffff"))

(defn make-transparent-rect [[x y]]
  (make-coloured-rect [x y] "none"))


(defn make-text [[x y] text]
  {:tag :text
   :attrs {:x (xstr x) :y (ystr y) :style "font-family:Verdana;font-size:24"}
   :content (list text)})

(defn make-high-text [[x y] text]
  {:tag :text
   :attrs {:x (xstr x) :y (yhighstr y) :style "font-family:Verdana;font-size:24"}
   :content (list text)})



(def grid (for [i (range 10) j (range 10)] (make-transparent-rect [i j])))



(defn yearlabels [century] (map (fn [y] (make-text (year->xy y) (str y))) (range (inc (* 100 (dec century))) (inc (* 100 century)) )))

(defn make-war [start end label labelyear] (concat (map (fn[y](make-red-rect (year->xy y))) (range start (inc end))) (list (make-high-text (year->xy labelyear) label))))



(def c20 (concat
           (make-war 1914 1918 "The Great War" 1918)
           (make-war 1939 1945 "The Second World War" 1941)
           (yearlabels 20)))

;; Making a simple one:
(def-let [svgfile
          (with-out-str
            (clojure.contrib.lazy-xml/emit
             {:tag :svg
              :attrs {:width "100%"
                      :height "100%"
                      :version "1.1"
                      :xmlns "http://www.w3.org/2000/svg"}
              :content  (concat grid c20)}))]
  (spit "file.svg" svgfile))





;;(clojure.xml/parse "file.svg")
;; Reading an svg image file
;;(pp/pprint (clojure.xml/parse "10x10.svg"))

;;(def labels (for [i (range 10)
;;                  j (range 10)]
;;              (make-text [i j] (str [i j]))))


;;(make-transparent-rect [10 10])
;;(make-white-rect [10 10])
;;(lazy-xml/emit (make-rect [0 0] "fill:#012345"))
;;(lazy-xml/emit (make-white-rect [0 0]))
;;(lazy-xml/emit (make-text [0 0] "hello"))

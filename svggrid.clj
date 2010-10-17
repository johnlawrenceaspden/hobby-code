(use ['clojure.contrib.lazy-xml :as 'lazy-xml])



(defn make-rect [[x y style]]
  {:tag :rect
   :attrs {:x (str x)
           :y (str y)
           :width "80"
           :height "80"
           :style style}})

(defn make-transparent-rect [[x y]]
  (make-rect [x y "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"]))

(defn make-coloured-rect [[x y colour]]
  (make-rect [x y (str "fill:"colour";stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1")]))


(defn make-red-rect [[x y]]
  (make-coloured-rect [x y "#ff0000"]))

(defn make-white-rect [[x y]]
  (make-coloured-rect [x y "#ffffff"]))



(make-transparent-rect [10 10])
(make-white-rect [10 10])
(lazy-xml/emit (make-rect [0 0 "fill:#012345"]))
(lazy-xml/emit (make-white-rect [0 0]))


(def grid (map make-white-rect (for [i (range 10)
                                           j (range 10)]
                                       [(* 80 i) (* 80 j)])))

;; Making a simple one:
(def-let [svgfile
          (with-out-str
            (clojure.contrib.lazy-xml/emit
             {:tag :svg
              :attrs {:width "100%"
                      :height "100%"
                      :version "1.1"
                      :xmlns "http://www.w3.org/2000/svg"}
              :content  (concat grid (list (make-red-rect [0 0])))}))]
  (spit "file.svg" svgfile))





;;(clojure.xml/parse "file.svg")
;; Reading an svg image file
;;(pp/pprint (clojure.xml/parse "10x10.svg"))

(use ['clojure.contrib.lazy-xml :as 'lazy-xml])


;; Michael Marczyk's example from
;; http://stackoverflow.com/questions/2463129/roundtripping-xml-in-clojure-using-clojure-xml-parse-and-clojure-xml-emit/2463768#2463768

(lazy-xml/emit
 (lazy-xml/parse-trim
  (java.io.StringReader. "<foo bar=\"&apos;&quot;&quot;&apos;\"/>")))

;; Reading an svg image file
(clojure.zip/xml-zip (clojure.xml/parse "/home/john/hobby-code/10x10.svg"))

;; Making a simple one:
(def-let [svgfile
          (with-out-str
            (clojure.contrib.lazy-xml/emit
             {:tag :svg
              :attrs {:width "100%"
                      :height "100%"
                      :version "1.1"
                      :xmlns "http://www.w3.org/2000/svg"}
              :content [ {:tag :rect,
                          :attrs
                          {:y "772.36218",
                           :x "760",
                           :height "80",
                           :width "80",
                           :id "rect2826-2-7-4-8-8-6",
                           :style
                           "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"}}
                         {:tag :rect
                          :attrs {:x "20" :y "20" :width "80" :height "80"}
                          :style "fill:none"}
                         {:tag :rect,
                          :attrs
                          {:y "532.36218",
                           :x "200",
                           :height "80",
                           :width "80",
                           :id "rect3602-9-0",
                           :style "fill:none;stroke:#000000;stroke-opacity:1"},
                          :content nil}]}))]
  (spit "file.svg" svgfile))

(clojure.xml/parse "file.svg")
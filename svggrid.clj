;; The other day, I wanted to make some graph paper.

;; While experimenting with inkscape, I noticed that svg is actually an xml file format.

;; Since clojure is good with xml, that means that it's actually easier to make such a drawing with a program:

;; Each element of the drawing is represented as a map

;; The library clojure.xml will turn the nested map into xml:
(require 'clojure.xml)

(defn make-rect [i j squaresize]
  {:tag :rect
   :attrs {:x (str (* squaresize i))
           :y (str (* squaresize j))
           :width  (str squaresize)
           :height (str squaresize)
           :style "fill:white;stroke:black;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"}})


(defn make-text [s]
  {:tag :text 
   :attrs {:a "hi"} :content s})

;; The whole file is represented as a map containing those maps

(defn make-svg [gridsize squaresize]
  {:tag :svg
   :attrs {:width "100%"
           :height "100%"
           :version "1.1"
           :xmlns "http://www.w3.org/2000/svg"}
   :content (for [i (range gridsize) j (range gridsize)] (make-rect i j squaresize))})


;; We can use with-out-str to capture the output, which is unaccountably printed
;; rather than given back as a string, and spit to write it to a file.

(spit "squares.svg" (with-out-str (clojure.xml/emit (make-svg 10 80))))

;; The nice thing about this is that you can then use inkscape to modify the
;; file, and then diff to work out how to take the modifications back into the
;; program. Does anybody know how to make the emit function format the xml so
;; that the output file is nicely readable?



;; There's also clojure.data.xml, which is a more annoying version of the same thing:

(require '[clojure.data.xml :as cdx])

;; It will parse existing xml
(def xml-picture "<svg xmlns=\"http://www.w3.org/2000/svg\">
<g>
  <rect width=\"100\" height=\"100\" fill=\"white\" stroke=\"black\" />
  <text x=\"20\" y=\"75\"  font-size=\"60\"  > 79 </text>
</g>
<g>
  <rect x=\"100\" y=\"100\" width=\"100\" height=\"100\" fill=\"white\" stroke=\"black\" />
  <text x=\"120\" y=\"175\"  font-size=\"60\"  > 68 </text>
</g>
</svg>")

;; Although for some reason parse-str throws away the xmlns bit

(cdx/parse-str xml-picture)

;; But we can construct the same text by hand like this

(def e cdx/element)

(def yo (cdx/indent-str (e :svg { :xmlns "http://www.w3.org/2000/svg" }
                          (e :g {} 
                             (e :rect {:x 0 :y 0   :width 100 :height 100 :fill "white" :stroke "black"})
                             (e :text {:x 20 :y 75 :font-size 60 } "79"))
                          (e :g {} 
                             (e :rect {:x 100 :y 100 :width 100 :height 100 :fill "white" :stroke "black"})
                             (e :text {:x 120 :y 175 :font-size 60 } "68")))))


;; Here's how to make numbered snakes and ladders boards


(defn make-square [i j n]
  (let [x (* 100 i) y (* 100 j)]
  (e :g {} 
     (e :rect {:x x :y y   :width 100 :height 100 :fill "white" :stroke "black"})
     (e :text {:x (+ x 20) :y (+ y 75) :font-size 60 } (str n)))))





(spit "slb1.svg" (cdx/indent-str (e :svg { :xmlns "http://www.w3.org/2000/svg" }
                                    (make-square 0 1 1)                          
                                    (make-square 1 1 2)
                                    (make-square 1 0 3)
                                    (make-square 0 0 4)                          
                                    )))

(spit "slb2.svg" (cdx/indent-str (e :svg { :xmlns "http://www.w3.org/2000/svg" }
                                    (map #(apply make-square %) '((0 1 1)(1 1 2 )(1 0 3)(0 0 4))))))




(defn slb [x y n stop dir size]
  (if (= n stop) (list (list x y n))
      (cons (list x y n)
            (if (= dir :right)
              (if (= (inc x) size) 
                (slb x (dec y) (inc n) stop :left  size)
                (slb (inc x) y (inc n) stop :right size))
              (if (= x 0) 
                (slb x (dec y) (inc n) stop :right size)
                (slb (dec x) y (inc n) stop :left size))))))

(spit "slb.svg" (cdx/indent-str (e :svg { :xmlns "http://www.w3.org/2000/svg" }
                                    (map #(apply make-square %) (slb 0 9 1 100 :right 10)))))
              

;; Here's the black death 1347-52

(defn make-black-square [i j n]
  (let [x (* 100 i) y (* 100 j)]
  (e :g {} 
     (e :rect {:x x :y y   :width 100 :height 100 :fill "black" :stroke "white"})
     (e :text {:x (+ x 20) :y (+ y 75) :font-size 60 :fill "white" } (str n)))))

(spit "black-death.svg" 
      (cdx/indent-str (e :svg { :xmlns "http://www.w3.org/2000/svg" }
                         (map #(if (<= 47 (nth % 2) 52) 
                                 (apply make-black-square %)
                                 (apply make-square %)) 
                              (slb 0 9 0 99 :right 10)))))











(with-out-str (cdx/emit {:tag :rect, :attrs {}} ))







(clojure.data.xml/parse-str xml-picture)

(def xml-picture "<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" 
\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">

<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">
  <circle cx=\"100\" cy=\"50\" r=\"40\" stroke=\"black\"  stroke-width=\"2\" fill=\"red\" />
</svg>")


(def xml-picture "<?xml version=\"1.0\"?>

<svg width=\"100%\" height=\"100%\" version=\"1.1\" 
     xmlns=\"http://www.w3.org/2000/svg\"
     xmlns:xlink=\"http://www.w3.org/1999/xlink\">

       <circle cx=\"100\" cy=\"50\" r=\"40\" stroke=\"black\"  stroke-width=\"2\" fill=\"red\" />
      
</svg>")


(do (def xml-picture "<svg  width=\"200\" height=\"200\" xmlns=\"http://www.w3.org/2000/svg\">
<circle cx=\"50\" cy=\"50\" r=\"50\" stroke=\"black\"  stroke-width=\"2\" fill=\"red\" />
</svg>")



    (def round-trip-picture (clojure.data.xml/indent-str
                             (clojure.data.xml/parse-str
                              xml-picture)))


    (spit "yo.svg" xml-picture)

    (spit "yo2.svg" round-trip-picture))


(require 'clojure.data.xml)
(let [ xml-picture "<svg  width=\"200\" height=\"200\" xmlns=\"http://www.w3.org/2000/svg\">
<circle cx=\"50\" cy=\"50\" r=\"50\" stroke=\"black\"  stroke-width=\"2\" fill=\"red\" />
</svg>"



      round-trip-picture (clojure.data.xml/indent-str
                             (clojure.data.xml/parse-str
                              xml-picture))]


  (spit "yo.svg" xml-picture)
  (spit "yo2.svg" round-trip-picture)
  (println xml-picture)
  (println "------------------")
  (println round-trip-picture))








#clojure.data.xml.Element{:tag :svg, :attrs {}, :content (#clojure.data.xml.Element{:tag :g, :attrs {}, :content (#clojure.data.xml.Element{:tag :rect, :attrs {:width "100", :height "100", :fill "white", :stroke "black"}, :content ()} #clojure.data.xml.Element{:tag :text, :attrs {:x "20", :y "75", :font-size "60"}, :content (" 79 ")})} #clojure.data.xml.Element{:tag :g, :attrs {}, :content (#clojure.data.xml.Element{:tag :rect, :attrs {:x "100", :y "100", :width "100", :height "100", :fill "white", :stroke "black"}, :content ()} #clojure.data.xml.Element{:tag :text, :attrs {:x "120", :y "175", :font-size "60"}, :content (" 68 ")})})}







(def e cdx/element)


(cdx/indent-str (e :tag {:foo-attr "foo value"}
                     (e :bar {:bar-attr "bar value"}
                       (e :baz {} "The baz value1")
                       (e :baz {} "The baz value2")
                       (e :baz {} "The baz value3"))))

<?xml version="1.0" encoding="UTF-8"?>
<foo foo-attr="foo value">
  <bar bar-attr="bar value">
    <baz>The baz value1</baz>
    <baz>The baz value2</baz>
    <baz>The baz value3</baz>
  </bar>
</foo>













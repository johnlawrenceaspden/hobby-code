;;Rob's butterfly puzzle

;;tile2 
;;going round clockwise

;;anticlockwise red admiral
;;clockwise small tortoiseshell
;;clockwise swallowtail
;;clockwise peacock
;;anticlockwise orange tip
;;anticlockwise brimstone
;;encodes as
;;(2 -r +t +s +p -o -b) 

(def r "red admiral")
(def t "small tortoiseshell")
(def s "swallowtail")
(def p "peacock")
(def o "orange tip")
(def b "brimstone")
(def anti "anticlockwise")
(def cloc "clockwise")

(def -r [anti r])
(def +r [cloc r])
(def -t [anti t])
(def +t [cloc t])
(def -s [anti s])
(def +s [cloc s])
(def -p [anti p])
(def +p [cloc p])
(def -o [anti o])
(def +o [cloc o])
(def -b [anti b])
(def +b [cloc b])

(defn matches [b1 b2]
  (and (not (= (b1 0) (b2 0))) (= (b1 1) (b2 1))))


(defstruct tile :number :edges)

(defn tile-perimeter-triple [position tile]
  (take 3 (drop (mod (- position 1) 6) (concat (tile :edges) (tile :edges)))))

(def tr "turn right")
(def tl "turn left")

(defstruct shape :perimeter :new-tile :ancestor  )

(defn shape-perimeter-length [shape] (/ (count (shape :perimeter)) 2))

(defn shape-perimeter-triple [position shape]
  (let [mp (mod (- position 1) (shape-perimeter-length shape))]
    (take 5 (drop (* 2 mp) (concat (shape :perimeter) (shape :perimeter))))))

(def empty-shape (struct-map shape :ancestor nil :new-tile nil :perimeter '[]))

(defn add-tile [new-tile position old-shape]
  (if (= old-shape empty-shape)
    (struct-map shape 
      :ancestor old-shape :new-tile new-tile 
      :perimeter (interleave (tile0 :edges) (repeat 6 tr)))))

(defn triples-compatible? [shape-perimeter-triple tile-perimeter-triple]
  (let [[fb ft cb st lb] shape-perimeter-triple
      [tfb tcb tlb] tile-perimeter-triple]
    (and (= ft tr) (= st tr) (matches tcb cb))))

(defn tile-and-shape-compatible-at-positions? [tile tposition shape sposition]
  (triples-compatible? (shape-perimeter-triple sposition shape)
                       (tile-perimeter-triple tposition tile)))

(defn tile-and-shape-compatibility [tile shape]
  (filter (fn [[m n]] (tile-and-shape-compatible-at-positions? tile m shape n))
          (for [m (range 6) n (range (shape-perimeter-length shape))] [m n]) ))


(def tiles 
     [(struct tile 0 [+b +r +s +o +p +t])
      (struct tile 1 [+b +o +s -r +p +t])
      (struct tile 2 [-b -r +t +s +p -o])])

;;tests
( = (map matches [+r +r +r +r] [+r -r +o +o]) '(false true false false))

(def tile0 (tiles 0))
(def tile1 (tiles 1))
(def tile2 (tiles 2))
(def shape0 (add-tile tile0 "dummy" empty-shape))

(def test-shapes [empty-shape shape0])

(map shape-perimeter-length test-shapes)
;(shape-perimeter-triple 0 empty-shape)
(shape-perimeter-triple 0 shape0)
(shape-perimeter-triple 1 shape0)
(tile-perimeter-triple 0 tile0)
(tile-and-shape-compatible-at-positions? tile0 0 shape0 0)
(= (tile-and-shape-compatibility tile0 shape0) '())
(= (tile-and-shape-compatibility tile1 shape0) '([3 1]))
(= (tile-and-shape-compatibility tile2 shape0) '([0 0] [1 1] [5 3]))

;--------------------scratch
shape0 {:perimeter (["clockwise" "brimstone"] "turn right" ["clockwise" "red admiral"] "turn right" ["clockwise" "swallowtail"] "turn right" ["clockwise" "orange tip"] "turn right" ["clockwise" "peacock"] "turn right" ["clockwise" "small tortoiseshell"] "turn right"), :new-tile {:number 0, :edges [["clockwise" "brimstone"] ["clockwise" "red admiral"] ["clockwise" "swallowtail"] ["clockwise" "orange tip"] ["clockwise" "peacock"] ["clockwise" "small tortoiseshell"]]}, :ancestor {:perimeter [], :new-tile nil, :ancestor nil}}
;;
tile2{:number 2, :edges [["anticlockwise" "brimstone"] ["anticlockwise" "red admiral"] ["clockwise" "small tortoiseshell"] ["clockwise" "swallowtail"] ["clockwise" "peacock"] ["anticlockwise" "orange tip"]]}

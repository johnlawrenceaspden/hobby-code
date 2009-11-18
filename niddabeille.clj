(use 'clojure.set)

;;boilerplate test code
(defmacro testset [title & tests]
  `(let [test# (and ~@tests)
         string# (str ~title " tests " (if test# "pass" "fail"))]
     (println string#)
     [(if test# true false), string#]))

(testset "test"(= (testset "arithmetic" (* 1 1)) [true "arithmetic tests pass"]))

;;to find the adjacencies when placing hexagons to make a nid d'abeille, 
;;19 hexagons themselves arranged in a hexagon.

;;We'll consider a quasi-square of hexagons numbered A1-E5

;;The nid hexes are:
;A2 A3 A4 B2 B3 B4 B5 C1 C2 C3 C4 C5 D2 D3 D4 D5 E2 E3 E4

;;numbering the cardinal directions 0-5
;;C3 is adjacent to 0:C2 1:D3 2:D4 3:C4 4:B4 5:B3

;numeric code for A B C D E is 1 2 3 4 5 so that we can use row numbers

(defn rowcol [str]
  "convert A3 to [1 3]"
  [(condp = (nth str 0) \A 1 \B 2 \C 3 \D 4 \E 5)
   (condp = (nth str 1) \1 1 \2 2 \3 3 \4 4 \5 5)])

(defn strrep [[row col]]
  "convert [1 3] to A3"
  (str (condp = row 1 \A 2 \B 3 \C 4 \D 5 \E)
       (condp = col 1 \1 2 \2 3 \3 4 \4 5 \5)))

(defn adjacencies [[row column]]
  "gets the neighbouring hexes by direction :0 to :5"
  (if (= 1 (mod row 2))
    {:0 [row (dec column)]
     :1 [(inc row) column]
     :2 [(inc row) (inc column)]
     :3 [row (inc column)]
     :4 [(dec row) (inc column)]
     :5 [(dec row) column]}

    {:0 [row (dec column)]
     :1 [(inc row) (dec column)]
     :2 [(inc row) column]
     :3 [row (inc column)]
     :4 [(dec row) column]
     :5 [(dec row) (dec column)]}))
    
;;the hexagons in the nid in a spiral ordering
(def nid 
     ["C3" 
      "C2" "D3" "D4" "C4" "B4" "B3" 
      "B2" "C1" "D2" "E2" "E3" "E4" "D5" "C5" "B5" "A4" "A3" "A2"])

(def rowcolnid (map rowcol nid))

(testset "hex geometry"
            (= (count nid) (count (distinct nid)) 19)
            (= (rowcol "A3") [1 3])
            (= (strrep [1 3]) "A3")
            (= (map strrep rowcolnid) nid)
            (= (adjacencies [3 3]) {:0 [3 2], :1 [4 3], :2 [4 4], :3 [3 4], :4 [2 4], :5 [2 3]})
            (= (for [[k v] (adjacencies (rowcol "C3"))] [k (strrep v)]) 
               '([:0 "C2"] [:1 "D3"] [:2 "D4"] [:3 "C4"] [:4 "B4"] [:5 "B3"]))
            (= (for [[k v] (adjacencies (rowcol "B3"))] [k (strrep v)])
               '([:0 "B2"] [:1 "C2"] [:2 "C3"] [:3 "B4"] [:4 "A3"] [:5 "A2"])))

;;this is the interesting function.
;;We're placing hexes in the order given by nid, (converted to rowcolnid)
;;For each new hex placed, we'd like to know which of its neighbours has already been placed, 
;;and which directions they're in. 

;;The recursion should go something like this:
;(adjchecks 0) 
;(adjchecks 1) [["C3",{}]]
;(adjchecks 2) [["C3",{}], ["C2",{3 "C3"}]]
;(adjchecks 3) [["C3",{}], ["C2",{3 "C3"}], ["D3" {4 "C3", 5 "C2"]]

(defn adj-checks [rowcolnid n]
  (if (= n 0) []
      (let []
        (let [newhex        (nth rowcolnid (dec n))
              previous      (adj-checks rowcolnid (dec n))
              previoushexes (map first previous)
              inprevioushexes (fn [v] (some #(= v %) previoushexes))]
          (concat previous  
                  [[newhex  
                    (for [[d v] (adjacencies newhex) :when (inprevioushexes v)] [d v])]] )))))


(testset "adjacency recursion"
         (= (adj-checks rowcolnid 0) [])
         (= (adj-checks rowcolnid 1) '([[3 3] ()]) )
         (= (adj-checks rowcolnid 2) '([[3 3] ()] [[3 2] ([:3 [3 3]])]) )
         (= (adj-checks rowcolnid 3) 
            '([[3 3] ()] [[3 2] ([:3 [3 3]])] [[4 3] ([:4 [3 3]] [:5 [3 2]])])))

(def nid-adjacencies (adj-checks rowcolnid (count rowcolnid)))

;;behold the adjacency list in its full glory, converted to "A3" notation for ease of reading

(def beesnest 
      (map (fn [[hex adjlist]] 
             [(strrep hex) 
              (map 
               (fn[[d hex]] [d (strrep hex)])
               adjlist)])
           nid-adjacencies))

(testset "beesnest"
         (= beesnest '(["C3" ()] 
                       ["C2" ([:3 "C3"])] 
                       ["D3" ([:4 "C3"] [:5 "C2"])] 
                       ["D4" ([:0 "D3"] [:5 "C3"])] 
                       ["C4" ([:0 "C3"] [:1 "D4"])] 
                       ["B4" ([:1 "C3"] [:2 "C4"])] 
                       ["B3" ([:1 "C2"] [:2 "C3"] [:3 "B4"])] 
                       ["B2" ([:2 "C2"] [:3 "B3"])] 
                       ["C1" ([:3 "C2"] [:4 "B2"])] 
                       ["D2" ([:3 "D3"] [:4 "C2"] [:5 "C1"])] 
                       ["E2" ([:4 "D3"] [:5 "D2"])] 
                       ["E3" ([:0 "E2"] [:4 "D4"] [:5 "D3"])] 
                       ["E4" ([:0 "E3"] [:5 "D4"])] 
                       ["D5" ([:0 "D4"] [:1 "E4"] [:5 "C4"])] 
                       ["C5" ([:0 "C4"] [:1 "D5"])] 
                       ["B5" ([:0 "B4"] [:1 "C4"] [:2 "C5"])] 
                       ["A4" ([:1 "B4"] [:2 "B5"])] 
                       ["A3" ([:1 "B3"] [:2 "B4"] [:3 "A4"])] 
                       ["A2" ([:1 "B2"] [:2 "B3"] [:3 "A3"])])))

(def hexnames (map first beesnest))

(def hex2num (zipmap hexnames (range (count hexnames))))
(def num2hex (zipmap (range (count hexnames)) hexnames))
(defn dir2num [d] (condp = d :0 0 :1 1 :2 2 :3 3 :4 4 :5 5))

(find hex2num "B2")

(def numericadjlist (map (fn [[hex adjlist]] 
                           [(hex2num hex) 
                            (map 
                             (fn[[d hex]] [(dir2num d) (hex2num hex)])
                             adjlist)])
                         beesnest))



;;the tiles

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


(def tiles 
     [(struct tile 0 [+b +r +s +o +p +t])
      (struct tile 1 [+b +o +s -r +p +t])
      (struct tile 2 [-b -r +t +s +p -o])
      (struct tile 3 [+b +p -t -o +s -r])
      (struct tile 4 [+b +t +p +o -r -s])
      (struct tile 5 [-b +r -p -t -s +o])
      (struct tile 6 [+b -o -t +p +s +r])
      (struct tile 7 [-b +p -o -t -s -r])
      (struct tile 8 [+b +p -o +r -t +s])
      (struct tile 9 [-b +t -s -p +o +r])
      (struct tile 10 [-b -s -o -p -r +t])
      (struct tile 11 [-b +r +t -o +p -s])
      (struct tile 12 [-b +t +p +s +o -r])
   (struct tile 13 [+b +p -t -s -r -o])
   (struct tile 14 [+b -p +o +r +t -s])
   (struct tile 15 [-b +s -p -o -t +r])
   (struct tile 16 [-b +o -t -p -r -s])
   (struct tile 17 [+b +o +r +s -p -t])
   (struct tile 18 [-b -p +r -o +s -t])


])

(defn tileedge [tile rotation edgedirection]
  ((tile :edges) (mod (- edgedirection rotation) 6)))

;;tests
(def tile0 (tiles 0))(def tile1 (tiles 1))(def tile2 (tiles 2))(def tile3 (tiles 3))
(def tile4 (tiles 4))(def tile5 (tiles 5))(def tile6 (tiles 6))(def tile7 (tiles 7))
(def tile8 (tiles 8))
(def tile9 (tiles 9))
(def tile10 (tiles 10))
(testset "tiles"
         ( = (map matches [+r +r +r +r] [+r -r +o +o]) '(false true false false))
         ( = (map (fn [n] (tileedge tile0 0 n)) (range 6)) (:edges tile0))
         ( = (map (fn [n] (tileedge tile0 n n)) (range 6)) (repeat 6 +b)))
  
(testset "preconditions" 
         (= numericadjlist '([0 ()] [1 ([3 0])] [2 ([4 0] [5 1])] 
                               [3 ([0 2] [5 0])] [4 ([0 0] [1 3])] [5 ([1 0] [2 4])] 
                                 [6 ([1 1] [2 0] [3 5])] [7 ([2 1] [3 6])] 
                                   [8 ([3 1] [4 7])] [9 ([3 2] [4 1] [5 8])] 
                                     [10 ([4 2] [5 9])] [11 ([0 10] [4 3] [5 2])] 
                                       [12 ([0 11] [5 3])] [13 ([0 3] [1 12] [5 4])] 
                                         [14 ([0 4] [1 13])] [15 ([0 5] [1 4] [2 14])] 
                                           [16 ([1 5] [2 15])] [17 ([1 6] [2 5] [3 16])] 
                                             [18 ([1 7] [2 6] [3 17])]))
         (= tile0 (struct tile 0 [+b +r +s +o +p +t]))
         (= tile1 (struct tile 1 [+b +o +s -r +p +t]))
         (= tile2 (struct tile 2 [-b -r +t +s +p -o])))

(defn anti [d]
  (mod (+ 3 d) 6))

(defn check-consistency [tilelist newtile rotation numericadjlist]
  (let [position (count tilelist) 
        newadjlist (nth numericadjlist position)]
    (every? identity 
            (map (fn [[d p]] 
                   (matches (tileedge newtile rotation d) 
                            (tileedge (first (nth tilelist p)) (second (nth tilelist p)) (anti d)))) 
                 (second newadjlist)))))

(testset "consistency"
         (= (map #(check-consistency [[tile0 1]] tile3 % numericadjlist) (range 6))
            '(false true false false false false))
         (= (map #(check-consistency [[tile0 1][tile3 1]] tile2 % numericadjlist) (range 6))
            '(false false false false false false))
         (= (map #(check-consistency [[tile0 0][tile5 0]] tile1 % numericadjlist) (range 6))
            '(false true false false false false)))

(defn ptile [tile] (tile :number))
(def tpos (fn[[tile rot]][(tile :number) rot]))
(def ptilelist (fn [tilelist] (map tpos tilelist)))
(defn tilelist->tiles [tilelist] (map (fn[[tile rot]] tile) tilelist))

(defn allpossiblerotations [tile]
  (map (fn[rot][tile rot]) (range 6)))

;(allpossiblerotations tile1) 


(defn tilelist->longertilelists [tilelist]
  (let [tilesused (tilelist->tiles tilelist)
        tilesnotused (vec (difference (set tiles)(set tilesused)))
        candidateswithrotations (mapcat allpossiblerotations tilesnotused)
        compatibles (filter 
                     (fn[[tile rot]](check-consistency tilelist tile rot numericadjlist)) 
                     candidateswithrotations)]
    (map (fn[trot] (conj tilelist trot)) compatibles)))
    

(def butterflies
  (memoize (fn [n]
  (if (= n 0) (map (fn[tile][[tile 0]]) tiles)
      (mapcat tilelist->longertilelists (butterflies (dec n)))))))


(comment
  (count (butterflies 18))
  (map ptilelist (butterflies 18))
  (map #(count (butterflies %)) (range 20)) ;;why are there twenty? only 19 tiles!
)


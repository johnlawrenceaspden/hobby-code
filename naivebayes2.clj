;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def new-guy (make-space-bar-patron))
(reduce disj new-guy [:martian :venusian])

#{:tall :green :fat}

(let [m
      (* 
       (probability :martian nil 1 1)    ; 3055/10002
       (probability :tall  :martian 1 1) ; 1089/1528
       (probability :green :martian 1 1) ; 2439/3056
       (probability :fat   :martian 1 1) ;  601/3056
       ) ; 1625564146635/47576846159872
      v
      (* 
       (probability :venusian nil 1 1) ; 6947/10002
       (probability :tall  :venusian 1 1) ; 884/1737
       (probability :green :venusian 1 1) ; 169/579
       (probability :fat   :venusian 1 1) ; 5629/6948
       ) ; 1460520058387/17472902391702
      ]
  [ (float (/ m (+ m v))) (float (/ v (+ m v))) ] ) ; [0.29015476 0.70984524]

;; 70% chance he's a venusian too

new-guy ; #{:tall :green :venusian :fat}

(defn classify [characteristics]
  (let [m (apply * (probability :martian nil 1 1)  (for [i characteristics] (probability i :martian 1 1)))
        v (apply * (probability :venusian nil 1 1) (for [i characteristics] (probability i :venusian 1 1)))]
    [ (float (/ m (+ m v))) (float (/ v (+ m v))) ]))


(def classified (for [i (range 100)]
                  (let [p (make-space-bar-patron)]
                    [(classify (reduce disj p [:martian :venusian])) p])))

(def sorted-classified (sort (fn [a b] (< (ffirst a) (ffirst b))) classified))

(def confidence-classes (partition-by first sorted-classified)) 


(partition-by identity  (map (fn [x] (if ((second x) :martian) ['m (first (first x))] ['v (second (first x))])) sorted-classified)) 



(let [c (second confidence-classes) mvlist (map (fn [x] (if ((second x) :martian) 'm 'v)) c)] [(ffirst c) (sort mvlist) (frequencies mvlist)] ) 
 [[0.040880386 0.9591196] (m v v v v v v v v v v v v v v v v v v v) {v 19, m 1}]



(map (fn [c] (let [mvlist (map (fn [x] (if ((second x) :martian) 'm 'v)) c)] [(ffirst c) (sort mvlist) (frequencies mvlist)] ))
     confidence-classes) 
 ([[0.017495114 0.9825049] (v v v v v v v v v v v v v v) {v 14}]
 [[0.040880386 0.9591196] (m v v v v v v v v v v v v v v v v v v v) {v 19, m 1}]
 [[0.14585963 0.85414034] (m m v v v v v v v v) {v 8, m 2}]
 [[0.23688416 0.7631158] (m m v v v v v v v v v v) {v 10, m 2}]
 [[0.29015476 0.70984524] (m m m v v v v v v v v v v) {v 10, m 3}]
 [[0.42628604 0.57371396] (m m m m v v v v) {m 4, v 4}]
 [[0.74855006 0.25144994] (m m m m m v) {v 1, m 5}]
 [[0.8769342 0.123065844] (m m m m m m m m m m m m m m m v v) {m 15, v 2}])
 
  
 



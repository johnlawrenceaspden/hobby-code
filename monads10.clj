(defmacro do-monad [[bind result] binding-vector expression]
  (if (< (count binding-vector) 2)
  `(~result ~expression)
  `(~bind ~(second binding-vector) (fn [~(first binding-vector)]
     (do-monad [~bind ~result] ~(drop 2 binding-vector) ~expression)))))

(def i-bind   (fn[v f] (f v)))
(def i-result (fn[x] x))

(do-monad [i-bind i-result]
          [a 5
           b (inc a)]
          (* a b))

(def m-bind (fn [value function] (if (nil? value) nil (function value))))
(def m-result (fn [x]x))          

(do-monad [m-bind m-result]
          [a 5
           b (inc a)]
          (* a b))

(def s-bind (fn [value function] (mapcat function value)))
(def s-result (fn [value] (list value)))

(do-monad [s-bind s-result]
          [a (range 5)
           b (range a)]
          (* a b))
          
(= (i-bind (i-result 2) inc) (inc 2))


(do-monad [i-bind i-result]
          [a 5
           b (inc a)]
          (* a b))

;;The monad laws
(= (i-bind (i-result 2) inc)   (inc 2))
(= (m-bind (m-result 2) inc)   (inc 2))
(= (s-bind (s-result 2) range) (range 2))

(= (i-bind 2 i-result)   2)
(= (m-bind nil m-result)   nil)
(= (s-bind '(1 2) s-result)  '(1 2))

(= (m-bind (m-bind 2 inc) -)
   (m-bind 2 (fn [x] (m-bind (inc x) -))))

(= (s-bind (s-bind '(2 3) range) list)
   (s-bind '(2 3) (fn [x] (s-bind (range x) list))))


;;;;;;;;;lifting

(defn nil-respecting-addition [x y z]
     (do-monad [m-bind m-result]
               [a x
                b y
                c z]
               (+ a b c)))


(map (fn [[a b c]] (nil-respecting-addition a b c))
     '([4 5 6][nil 5 6][4 nil 6][4 5 nil][4 nil nil][nil 5 nil][nil nil 6][nil nil nil]))

(defn seq-addition [x y]
     (do-monad [s-bind s-result]
               [a x
                b y]            
               (+ a b)))

(seq-addition '(1 2 3) '(100 200 300))

;;;;;;;;;;;;;m-seq

(comment (m-seq [(range 3) '(1 2) '(\a \b \c)]))

(do-monad [s-bind s-result]
  [x (range 3)
   y '(1 2)
   z '(\a \b \c)]
  (list x y z))

;;m-chain

(comment (m-chain [range range range]))

(def m-chain-rrr 
     (fn [arg]
       (do-monad [s-bind s-result]
        [x (range arg)
         y (range x)
         z (range y)]
        z)))

(m-chain-rrr 6)
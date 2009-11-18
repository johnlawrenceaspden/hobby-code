(use 'clojure.contrib.monads)

(for [a (range 5)
      b (range a)]
  (* a b))

(domonad sequence-m
         [a (range 5)
          b (range a)]
         (* a b))


(mapcat (fn[a] 
          (mapcat (fn[b] 
                    (list (* a b)))
                  (range a)))
        (range 5) )

(defn sequence-bind [sequence function]
  (mapcat function sequence))

(defn sequence-result [value]
  (list value))

(sequence-bind (range 5) (fn [a]
   (sequence-bind (range a) (fn [b]
      (sequence-result (* a b))))))

;;one of the monad axioms?
(mapcat list (range 5))
;;(bind expression result) = expression
;;nb bind is mapcat with the arguments reversed
(=(sequence-bind (range 5) sequence-result) (range 5))
(=(sequence-bind '(a b c) sequence-result) '(a b c))

(def nil-respecting-addition
     (with-monad maybe-m
       (m-lift 2 +)))

(nil-respecting-addition 2 3)
(nil-respecting-addition 2 nil)
(nil-respecting-addition nil 3)
(nil-respecting-addition nil nil)

(defn my-nil-respecting-addition [x y]
     (domonad maybe-m
              [a x
               b y]
              (+ a b)))

(map nil-respecting-addition '(2 2 nil nil) '(2 nil 3 nil))

(defn yet-another-nil-respecting-addition [x y]
  (if (nil? x) nil
      (let [a x]
        (if (nil? y) nil
            (let [b y]
              (+ a b))))))

(map yet-another-nil-respecting-addition '(2 2 nil nil) '(2 nil 3 nil))

(defn sequence-addition [x y]
  (domonad sequence-m
           [a x
            b y]
           (+ a b)))

(sequence-addition '(1 2 3) '(100 200 300))

(with-monad identity-m
  (defn mystery [f xs]
    ((m-lift 1 f) xs)))

(mystery #(* 2 %) 1)
((fn[f v](f v)) #(* 2 %) 1)

(with-monad sequence-m
  (defn mystery [f xs]
    ((m-lift 1 f) xs)))

(mystery #(* 2 %) '(1 2 3))
(map #(* 2 %) '(1 2 3))

(with-monad maybe-m
  (defn mystery [f xs]
    ((m-lift 1 f) xs)))

(mystery - nil)
(mystery - 5)
(map (fn[x] (if (nil? x) nil (- x))) '(5 nil))


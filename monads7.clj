(for [a (range 5)]
  (* a a))

(map (fn[a] (* a a)) (range 5))

(mapcat (fn[a] (list (* a a))) (range 5))

(defn s-bind [sequence function]
  (mapcat function sequence))

(defn s-result [value]
  (list value))

(s-bind (range 5) (fn [a]
                   (s-result (* a a))))

(s-bind (range 5) (fn [b]
  (s-bind (range b) (fn [a]
    (s-result (* a a))))))

(for [b (range 5)
      a (range b)]
      (* a a))

(defmacro my-for [[a arg1] exp]
  `(s-bind ~arg1 (fn [~a]
                  (s-result ~exp))))

(my-for [x (range 5)]
        (* x x))

(defmacro my-for [binding-vector exp]
  (if (> (count binding-vector) 1)
    `(s-bind ~(second binding-vector) (fn [~(first binding-vector)]
                                        (my-for ~(drop 2 binding-vector) ~exp)))
    `(s-result ~exp)))

(macroexpand '(my-for [x (range 5)] (* x x)))

(my-for [x (range 5)]
        (* x x))

(my-for [x (range 5)
         y (range x)
         z (list (* y y))
         w (list (* x x))]
        (list x y z w))
        
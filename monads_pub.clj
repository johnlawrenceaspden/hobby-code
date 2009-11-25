(defmacro do-monad [[binder result] bindings expression]
  (if (= 0 (count bindings))
    `(~result ~expression)
    `(~binder ~(second bindings) (fn[~(first bindings)]
       (do-monad [~binder ~result] ~(drop 2 bindings) ~expression)))))

(let [a 2
      b (inc a)]
  (* a b))

(use 'clojure.contrib.monads)

(domonad identity-m
         [a 1
          b (inc a)]
         b)

(for [a (range 10)
      b (range a)]
  [a,b])

(domonad sequence-m
         [a (range 10)
          b (range a)]
         [a,b])

(range 2)
(inc 4)

(domonad set-m
         [a '(1 2 3 3 2 1)
          b (range a)]
         [a,b])


(let [a 2
      b (inc a)]
  (* a b))


((fn[a] ((fn[b] (* a b)) (inc a) )) 2)


((fn[a] 
   ((fn[b] 
      (* a b)) 
    (inc a)))
 2)

(defn bind [value function]
  (function value))

(defn return [value]
  value)

     (bind 2       (fn[a]
     (bind (inc a) (fn[b]
              (return (* a b))))))


(defn bind-map [value function]
  (mapcat function value))

(defn return-map [value]
  (list value))

(bind-map (range 10)     (fn [a]
 (bind-map (range a)     (fn [b]
                       (return-map [ a, b]) ))))

(for [a (range 10)
      b (range a)]
  [a,b])


(defn bind-set [value function]
  (apply sorted-set (mapcat function value)))

(defn return-set [value]
  (apply sorted-set (list value)))

(bind-set (range 10)     (fn [a]
 (bind-set (range a)     (fn [b]
                       (return-set (* a b)) ))))


(set '(1 2 3))
(sorted-set 1 2 3)




(def integers (partition 2 '[1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 12 "twelve"]))

(defn swap
  ([x] (swap x integers))
  ([x lst]
     (if (=(count lst) 0) nil
         (let [[a s] (first lst)]
           (if (= x a) s
               (if (= x s) a
                   (recur x (rest lst))))))))


(swap "one")
(swap 1)

(swap "eleven")

(defn pythagoras [xw yw]
  (let [x (swap xw)
        y (swap yw)
        r2 (+ (* x x) (* y y))
        r (Math/sqrt r2)
        rw (swap r)]
    rw))

(pythagoras "three" "four")
(pythagoras "four" "five")
(pythagoras "four" "six")

(defn pythagoras [xw yw]
  (let [x (swap xw)]
    (if (nil? x) nil
        (let [y (swap yw)]
              (if (nil? y) nil
                  (let [r2 (+ (* x x) (* y y))]
                    (let [r (Math/sqrt r2)]
                      (let [rw (swap r)]
                        rw))))))))


(defn maybe-bind [value function]
    (if (nil? value) nil
        (function value)))


(maybe-bind (swap "three")     (fn [a]
 (maybe-bind (swap "six")     (fn [b]
                       (identity (+ a b))))))



(defn pythagoras [xw yw]
  (let [x (swap xw)
        y (swap yw)
        r2 (+ (* x x) (* y y))
        r (Math/sqrt r2)
        rw (swap r)]
    rw))

(defn pythagoras [xw yw]
  (maybe-bind (swap xw)               (fn[x]
    (maybe-bind (swap yw)             (fn[y]
      (maybe-bind (+ (* x x) (* y y)) (fn[r2]
       (maybe-bind (Math/sqrt r2)     (fn[r]
        (maybe-bind (swap r)          (fn[rw]
                                        rw)))))))))))

(defn pythagoras [xw yw]
  (do-monad [maybe-bind identity] [x (swap xw)
                                  y (swap yw)
                                  r2 (+ (* x x) (* y y))
                                  r (Math/sqrt r2)
                                  rw (swap r)]
           rw))



(macroexpand  '(do-monad [maybe-bind identity] [x (swap xw)
                                  y (swap yw)
                                  r2 (+ (* x x) (* y y))
                                  r (Math/sqrt r2)
                                  rw (swap r)]
           rw))



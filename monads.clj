(use 'clojure.contrib.monads)

;;consider let lambda equivalence

(let [a 2 
      b (inc a)]
  (* a b))

;;is easy to read. But under the hood, let statements are function applications

'(let [arg val] exp)

;;is precisely equivalent to 

'((fn[arg] exp) 1)

;;and since

'(let [arg1 val1 arg2 val2] exp)

;;is equivalent to
'(let [arg1 val1] (let [arg2 val2] exp))

;;the whole thing is equivalent to 
'((fn [arg1] (let [arg2 val2] exp)) val1)

;;and then
'((fn [arg1] ((fn[arg2] exp) val2)) val1)

;;in other words, clojure's let is actually a convenient shorthand for a chain of function calls

(let [a 2 
      b (inc a)]
  (* a b))

;;is the same as

((fn[a] 
   ((fn[b] 
      (* a b)) 
    (inc a))) 
 2)

;;The second form is harder to read, because the values 2 and (inc a) have gone
;;away from the bindings a and b

;;now let us imagine a function called bind
(defn bind [value function]
  (function value))

;;which enables us to write:

(bind 2       (fn [a]
(bind (inc a) (fn [b]
   (* a b)))))

;;It's not as pretty as let, but it allows us to do the same sort of thing without too much 
;;effort.

(domonad identity-m
         [a 2
          b (inc a)]
         (* a b))

;;macro expansion of how domonad works

(macroexpand-1 
 '(domonad identity-m
           [a 2
            b (inc a)]
           (* a b)))

(clojure.contrib.monads/with-monad identity-m 
  (m-bind 2       (fn [a] 
  (m-bind (inc a) (fn [b] 
     (m-result (* a b)))))))

(let* [name__4454__auto__ identity-m 
       m-bind   (:m-bind name__4454__auto__) 
       m-result (:m-result name__4454__auto__) 
       m-zero   (:m-zero name__4454__auto__) 
       m-plus   (:m-plus name__4454__auto__)] 
      (clojure.contrib.macro-utils/with-symbol-macros 
        (m-bind 2       (fn [a] 
        (m-bind (inc a) (fn [b] 
       (m-result (* a b))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;The sequence monad
;;Consider for

(for [a (range 5)
      b (range a)]
  (* a b))

(domonad sequence-m
         [a (range 5)
          b (range a)]
         (* a b))

(clojure.contrib.monads/with-monad sequence-m 
  (m-bind (range 5) (fn [a] 
  (m-bind (range a) (fn [b] 
       (m-result (* a b)))))))

(defn bind [sequence function]
  (apply concat (map function sequence)))

(defn result [value]
  (list value))

(bind (range 5) (fn [a]
 (bind (range a) (fn [b]
     (result (* a b))))))


(apply concat (map (fn [a]
                     (apply concat (map (fn [b]
                                          (list (* a b)))
                                        (range a)))
                     (range 5))


(map (fn [a] 
       (map (fn [b] (* a b))
              (range a)))
       (range 5))

(range 5)

(concat
 (apply concat (map (fn [b] (list (* 0 b))) (range 0)))
 (apply concat (map (fn [b] (list (* 1 b))) (range 1)))
 (apply concat (map (fn [b] (list (* 2 b))) (range 2)))
 (apply concat (map (fn [b] (list (* 3 b))) (range 3)))
 (apply concat (map (fn [b] (list (* 4 b))) (range 4))))

(range 5)
(map (fn[a] (apply concat (map (fn[b] (list (* a b))) (range a)))) (range 5))

(apply concat (map (fn[a] 
  (apply concat (map (fn[b] 
    (list (* a b))) 
    (range a)))) 
  (range 5)))

(defn bind [sequence function]
  (apply concat (map function sequence)))

(defn result [value]
  (list value))

(bind (range 5) (fn[a] 
  (bind (range a) (fn[b] 
    (result (* a b))))))

(bind (range 5) (fn[a] 
  (bind (range a) (fn[b]
    (bind (range b) (fn [c]
      (result (vec a b c))))))))

(for [a (range 5)
      b (range a)
      c (range b)]
  (vec (list a b c)))

(vec (list 1 2 3))




(apply concat (map (fn [a]
              (apply concat (map (fn [b]
                            (list (* a b)))))
                      (range a)))
              (range 5))




;;let<-->lambda
((fn[a] exp) val) 
<-->
(let [a val] exp)

;;ll example
(let [a 2] (* a (inc a)))
-->
((fn[a] (* a (inc a))) 2)

;;harder example
(let [a 2 b 3] (* a b))
<-->
(let [a 2] (let [b 3] (* a b)))
<-->
(let [a 2] ((fn[b] (* a b))3) )
<-->
((fn[a] ((fn [b] (* a b)) 3)) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;part 2
 
(for [a (range 5)
      b (range a)]
  (* a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map inc '(1 2 3)) ;map fn list -> list
(2 3 4)
(map inc #{1 2 3}) ;map fn set -> list
(2 3 4)

(doc parents)

(class [])
clojure.lang.PersistentVector
(parents (class []))
#{clojure.lang.APersistentVector}
(map parents (parents (class [])))
(#{java.lang.Iterable clojure.lang.Streamable java.util.RandomAccess clojure.lang.AFn java.lang.Comparable java.util.List clojure.lang.IPersistentVector})


(map parents (parents (class [])))


(with-monad sequence-m
  (defn n-th-generation
    [n cls]
    ( (m-chain (replicate n parents)) cls )))

(class [])
(n-th-generation 0 (class []))
(n-th-generation 1 (class []))

(map #(n-th-generation % (class [])) (range 10))

(with-monad set-m
  (defn n-th-generation-set
    [n cls]
    ( (m-chain (replicate n parents)) cls )))

(n-th-generation-set 0 (class []))
(n-th-generation-set 1 (class []))
(n-th-generation-set 2 (class []))
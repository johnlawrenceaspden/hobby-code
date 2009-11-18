(defn fib [n]
  (condp = n
    0 0
    1 1
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 10)

(defn fluff [m n]
  (cond
    (and (= m 0) (= n 0)) 0 
    (= m 0) 1
    (= n 0) 1
    :else (+ (fluff (- m 1) n) (fluff (- n 1) m))))

(def fluff (memoize fluff))
(fluff 50 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro debug [x]
  `(println '~x \= ~x))
 

(gen-class 'UserException :extends Exception :load-impl-ns true)


(defn safe-get [a b]
  (let [x (get a b)]
    (if (nil? x)
      (throw (UserException . (format "%s is not in %s" b a)))
      x)))

(safe-get {:a 2} :a)
(safe-get {:a 2} :b)

(match-mapping '[a [b [unquote-seq x]] [unquote y]] 
               '[a [b c d] [c d e f]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defn match-mapping "Return a map of variable bindings for this matching, or 
                     throw an error if a matching is not possible."
  [var-tree match-tree]
  (cond (or (not (coll? var-tree)) (empty? var-tree))
         (when (not= var-tree match-tree)
           (throw (Exception. (str "Bad Match: " var-tree " " match-tree))))
        (= (first var-tree) 'unquote)
         {(second var-tree) match-tree}
        (and (coll? (first var-tree)) (= (ffirst var-tree) 'unquote-seq))
         {(second (first var-tree)) match-tree}
        (not (coll? match-tree))
         (throw (Exception. (str "Bad Match: " var-tree " " match-tree)))
        :else 
         (merge (match-mapping (first var-tree) (first match-tree))
                (match-mapping (rest var-tree)  (rest  match-tree)))))

(match-mapping '[a [b [unquote-seq x]] [unquote y]] 
               '[a [b c d] [c d e f]])
(match-mapping '[] '[])

(defn match-vars   "Return a seq of the variables mentioned in the tree."
  [var-tree]
  (cond (not (coll? var-tree)) nil
        (empty? var-tree) nil
        (= (first var-tree) 'unquote) [(second var-tree)]
        (and (coll? (first var-tree)) (= (ffirst var-tree) 'unquote-seq)) [(second (first var-tree))]
        :else (concat (match-vars (first var-tree)) (match-vars (rest var-tree)))))


(= (match-vars nil) nil)
(= (match-vars '[unquote x]) '[x])
(= (match-vars 'x) nil)
(= (match-vars '[]) nil)
(= (match-vars '[x]) '())
(= (match-vars '[x y [unquote x] z]) '(x))



(defmacro match "Take a var-tree with (unquote x) and (unquote-seq y) expressions
                 and match it with match-tree, binding these variables, and
                 throwing an exception if a valid matching cannot be found."
  [[var-tree match-tree] & body]
  (let [vars (match-vars var-tree) g (gensym)]
    `(let [~g (match-mapping '~var-tree ~match-tree)]
       (let ~(apply vector (mapcat #(vector % `(safe-get ~g '~%))  vars))
          ~@body))))
 
(match [[a [b [unquote-seq x]] [unquote y]] 
                   '[a [b c d] [c d e f]]] 
             [x y])

(macroexpand '(match [[a [b [unquote-seq x]] [unquote y]] 
                   '[a [b c d] [c d e f]]] 
             [x y]))

(let* [G__2863 (match-mapping 
                (quote [a [b [unquote-seq x]] [unquote y]]) 
                (quote [a [b c d] [c d e f]]))] 
      (let [x (get G__2863 (quote x)) 
            y (get G__2863 (quote y))] [x y]))







(match-vars '[a [b [unquote-seq x]] [unquote y]])
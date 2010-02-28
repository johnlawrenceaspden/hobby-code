;; Nice macros

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;; Examples of dbg
(println (+ (* 2 3) (dbg (* 8 9))))
(println (dbg (println "yo")))
(defn factorial[n] (if (= n 0) 1 (* n (dbg (factorial (dec n))))))
(factorial 8)

(def integers (iterate inc 0))
(def squares  (map #(dbg(* % %))   integers))
(def cubes    (map #(dbg(* %1 %2)) integers squares))

;; three armed if with tolerances
(defmacro tif [x z p n]
  `(let [x# ~x]
     (cond (<= x# -0.0001)       ~n
           (< -0.0001 x# 0.0001) ~z
           (<= 0.0001 x#)        ~p)))

(map #(tif % 'roughly-zero (/ 10 %) (/ -10 %)) 
     '(-1 -0.001 -0.00001 0 0.0001 0.001 1))


;;can we do it without backquote?
(defmacro pyth1[a b]
  (let [a# (gensym)
        b# (gensym)]
     (list 
      'let (vector a# a b# b)
     (list '+ (list '* a# a#) (list '* b# b#)))))

(defmacro pyth2[a b]
  `(let [a# ~a b# ~b]
     (+ (* a# a#) (* b# b#))))


(clojure.walk/macroexpand-all '(* (pyth1 2 3) (pyth2 2 3)))

(* 
 ;;hand generated version
 (let* [G__3409 2 G__3410 3] 
       (+ (* G__3409 G__3409) (* G__3410 G__3410)))
 ;;auto version. note name resolution
 (let* [a__3360__auto__ 2 b__3361__auto__ 3] 
       (clojure.core/+ 
        (clojure.core/* a__3360__auto__ a__3360__auto__) 
        (clojure.core/* b__3361__auto__ b__3361__auto__))))


(binding [list vector]
  (list 1 3))
(binding [list +]
  (list 1 3))
(binding [+ list]
  (+ 1 3))


(condp = 
  1 'one
  2 'two
  'many)

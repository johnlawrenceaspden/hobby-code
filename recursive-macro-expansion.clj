;; macros are code transformers
;; in a lisp, that means tree transformers
;; let's look at the -> macro, built into clojure.

(use 'clojure.contrib.repl-utils)
(source ->)

;; (defmacro ->
;;   "Threads the expr through the forms. Inserts x as the
;;   second item in the first form, making a list of it if it is not a
;;   list already. If there are more forms, inserts the first form as the
;;   second item in second form, etc."
;;   ([x] x)
;;   ([x form] (if (seq? form)
;;               (with-meta `(~(first form) ~x ~@(next form)) (meta form))
;;               (list form x)))
;;   ([x form & more] `(-> (-> ~x ~form) ~@more)))



;; What's 20C in Fahrenheit?
;; Multiply by nine, divide by 5, add 32
(-> 20 (* 9) (/ 5) (+ 32))

;; let's look at the tree for that
(use 'clojure.inspector)
(inspect-tree '(-> 20 (* 9) (/ 5) (+ 32)))

;; What does that look like after macroexpansion?
(use 'clojure.walk)
(macroexpand-all '(-> 20 (* 9) (/ 5) (+ 32)))
(inspect-tree (macroexpand-all '(-> 20 (* 9) (/ 5) (+ 32))))


;; How does the threading macro work?
(macroexpand-1 '(-> 20 (* 9) (/ 5) (+ 32)))

;; We can follow the recursion by using macroexpand-1 on each subexpression in turn.

;;slightly tidied up, stages are:

(macroexpand-1 '(-> 20 (* 9) (/ 5) (+ 32)))

(macroexpand-1
'(-> 
  (-> 20 (* 9))
  (/ 5) (+ 32)))

(macroexpand-1
 '(-> 
   (-> 
    (-> 20 (* 9)) 
    (/ 5)) 
   (+ 32)))

(macroexpand-1
 '(+ 
   (-> 
    (-> 20 (* 9)) 
    (/ 5))
   32))

`(+ 
  ~(macroexpand-1 '
    (-> 
     (-> 20 (* 9)) 
     (/ 5)))
  32)

`(+ 
  (/ 
   ~(macroexpand-1 '
     (-> 20 (* 9)))
   5)
  32)

(+ 
 (/ 
  (* 20 9) 
  5) 
 32)

;;This is a bit long-winded, how would we do this automatically?

(defn slow-tree-transformer [fn tree]
  (cond (not (seq? tree)) (fn tree)
        (empty? tree) '()
        :else (let [nxt (fn tree)]
                (if (not (= nxt tree)) nxt
                    (let [nxt (fn (first tree))]
                      (if (= nxt (first tree))
                        (let [nxt (slow-tree-transformer fn (first tree))]
                          (if (= nxt (first tree))
                            (cons (first tree) (slow-tree-transformer fn (rest tree)))
                            (cons nxt (rest tree))))
                        (cons nxt (rest tree))))))))


(def f (fn[x] (slow-tree-transformer macroexpand-1 x)))

(defn iterate-to-stable [fn start]
  (let [next (fn start)]
    (if (= next start) (list start)
        (cons start (iterate-to-stable fn next)))))

(use 'clojure.contrib.pprint)
(println "-----------------------------------")
(doall (map (fn [x] (pprint x) (println)) (iterate-to-stable f '(-> 20 (* 9) (/ 5) (+ 32)))))
(println "-----------------------------------")





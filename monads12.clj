;;The sequence monad

;;What else could we use in place of bind?

;;Consider

(let [a 1]
  (let [b 2]
    (* a b)))

;;which we've already seen is the same as:

((fn [a]
   ((fn [b]
      (* a b))
    2))
 1)

;;What about map?

(map (fn [c]
       (map (fn[b]
              (map (fn[a] 
                     (+ a b c))
                   '(1 2 3)))
            '(10 20 30)))
       '(100 200 300))

(defmacro with-binder [binder bindings expression]
  (if (= 0 (count bindings))
    expression
    `(~binder ~(second bindings) (fn[~(first bindings)]
                                (with-binder ~binder ~(drop 2 bindings) ~expression)))))

(defn f-bind [sequence function]
  (map function sequence))

(with-binder f-bind
  [c '(100 200 300)
   b '(10 20 30)
   a '(1 2 3)]
  (+ a b c))

;;It works nicely, but the result is a deeply nested sequence
;;If we use mapcat instead for the outer applications, then
;;we get something that takes sequences and produces sequences.
(mapcat (fn [c]
       (mapcat (fn[b]
              (map (fn[a] 
                     (+ a b c))
                   '(1 2 3)))
            '(10 20 30)))
       '(100 200 300))

;;We'll have to change the macro to take two different bind functions, 
;;one for the innermost only loop.

;;Another approach is to introduce a function which makes a sequence out of a value
(mapcat (fn [c]
       (mapcat (fn[b]
              (mapcat (fn[a] 
                     (list (+ a b c)))
                   '(1 2 3)))
            '(10 20 30)))
       '(100 200 300))

;;It's not at all clear to me why this approach would be preferable, but it's the monadic way
;;Here's our new macro:
(defmacro do-monad [[binder result] bindings expression]
  (if (= 0 (count bindings))
    `(~result ~expression)
    `(~binder ~(second bindings) (fn[~(first bindings)]
       (do-monad [~binder ~result] ~(drop 2 bindings) ~expression)))))

;;The monad is the pair of functions bind and result.
(defn f-bind [sequence function]
  (mapcat function sequence))
(defn f-result [value]
  (list value))

(do-monad [f-bind f-result]
          [a '(1 2 3)
           b '(10 20 30)
           c '(100 200 300)]
          (+ a b c))

;;With our new monad, we've recreated the functional for loop
(for [a '(1 2 3) 
      b '(10 20 30) 
      c '(100 200 300)] 
  (+ a b c))

;;remember that we have the earlier names available lower down
(do-monad [f-bind f-result]
          [end  (range 6)
           begin (range end)
           second (range (inc begin) end)
           third (range (inc second) end)]
          (list begin second third end))


;;We can instantly create another monad, using sets instead of lists
(defn s-bind [sequence function]
  (set (mapcat function sequence)))
(defn s-result [value]
  (set (list value)))

(do-monad [s-bind s-result]
          [a '(1 2 3)
           b '(1 2 3)
           c '(1 2 3)]
          (+ a b c))

;;Using the identity monad, or let, we can chain functions into arbitrary nets

;;Using the maybe monad, we can chain arbitrary functions which take values
;;but produce either values or nil.

;;Using the sequence monad, we can chain functions which take values and produce sequences
;;arbitrary computational nets. 

;;We can think of using the set monad as chaining multiple valued functions.

;;What else can we do?

;;Suppose we have some functions which let us know what they have done.
;;Let's call them 'debuggable functions'

(defn d+ [a b] [(+ a b), (format "(+ %s %s);" a b)])
(defn d* [a b] [(* a b), (format "(* %s %s);" a b)])

(d+ 1 2)
(d* 2 3)

;;In fact, we can make a macro which turns arbitrary functions into debuggable functions:

(defmacro dbg-fn [function]
  `(fn [& vals# ] [(apply ~function vals#)(str (cons '~function vals#) )]))

(def d+ (dbg-fn +))
(def d* (dbg-fn *))
(def dlist (dbg-fn list))
(d+ 1 2 3)
(dlist 1 2 3)
(d* 1 2 3 4)

;;Unfortunately, these can no longer be combined
;; (d+ ( d+ 1 2) 3) is an error

;;What we'd like to write is something like:
(debug-let [a (d+ 1 2)
            b (d+ a 3)
            c (d+ a b)]
           c)

;;We may be able to define dbg-bind and dbg-result so that we can use:
(do-monad [dbg-bind dbg-result]
                         [a (d+ 1 2)
                          b (d+ a 3)
                          c (d+ a b)]
                         c)

;;We know that this macro will expand into:
'(dbg-bind (d+ 1 2) (fn [a] 
    (dbg-bind (d+ a 3) (fn [b] 
       (dbg-bind (d+ a b) (fn [c] 
         (dbg-result c)))))))


;;dbg-result obviously has to turn a value into a [value string] pair.
;;So we might try:
(defn dbg-result [value]
  [value ""])
;;dbg-bind must take:
;; a [value string] pair, and 
;; a function which takes a value and returns a value and a string
;;and it must return a pair of the final value and the concatenation of the two strings.
;;Once we've stated the problem, it can really only be:
(defn dbg-bind [[value string] function]
  (let [[r s] (function value)]
    [r (str string s)]))

;;Here's an example:
(do-monad [dbg-bind dbg-result]
                         [a (d+ 1 2)
                          b (d+ a 3)
                          c (d+ a b)
                          d (d* b c)
                          e (dlist a b c d)
                          f ((dbg-fn print) a b c d e)]
                         (list a b c d f))


(do-monad [dbg-bind dbg-result]
                         [x (dbg-result 1)
                          y (dbg-result 2)
                          z (dbg-result 3)
                          a (d+ x y)
                          b (d+ a z)
                          c (d+ a b)
                          d (d* b c)
                          e ((dbg-fn /) d c)]
                         e)


;;What monads are built into clojure.contrib.monads?
(filter 
 (fn[x] (not (nil? (re-matches #".*-m" x))))   
 (map 
  (fn[x] (str (first x))) 
  (ns-interns 'clojure.contrib.monads)))

;;python syntax would be
[x for x in 
 [(str (first x)) for x in (ns-interns 'clojure.contrib.monads)] 
 if (not (nil?(re-matches #".*-m" x)))]

;;monad-syntax
(domonad sequence-m
         [x (ns-interns 'clojure.contrib.monads)
          s (m-result (str (first x))) 
          s-m (m-result s) :when (not (nil? (re-matches #".*-m" s)))]
         s-m)


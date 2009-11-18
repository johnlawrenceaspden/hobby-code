;;Monads

;;Suppose we have two functions, bind and unit

(defn i-bind [value function]
  (function value))
(defn i-unit [value]
  value)

;;Then we can use them to express the idea of binding names
;;to values:

;;We might express the idea
;; let a=1 and
;; let b=(inc a) and
;; let c=(+ a b)
;; in [a b c]
;;as

(i-bind 1 (fn [a]
        (i-bind (inc a) (fn [b]
                (i-bind (+ a b) (fn [c]
                        (i-unit [a b c])))))))

;;This syntax is unreadable, so we define a macro:
(defmacro do-monad [[bind unit] bindings & body]
  (if (= 0 (count bindings)) 
    `(~unit ~@body)
    (let [[name# value# & brest#] bindings]
          `(~bind ~value# (fn[~name#] (do-monad [~bind ~unit] ~brest# ~@body))))))


;;which allows us to write, by analogy with let:
(do-monad [i-bind i-unit]
          [a 1
           b (inc a)
           c (+ a b)]
          [a b c])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;There are other uses apart from recreating the let form.
;;One is to recreate the for form!

(defn s-bind [value function]
  (mapcat function value))
(defn s-unit [value]
  (list value))

(do-monad [s-bind s-unit]
    [a '(1 2 3)
     b (range (inc a))]
  (* a b))

;;Another is to create a way of making error-tolerating functions
(defn m-bind [value function]
  (if (nil? value) nil (function value)))
(defn m-unit [value]
  value)

(def str->num {"one" 1 "two" 2})
(def num->str {1 "one" 2 "two"})
(defn str-add [as bs] 
  (do-monad [m-bind m-unit]
      [a  ( str->num as)
       b  ( str->num bs)
       cs ( num->str (+ a b))]
    cs))

(str-add "one" "one")
(str-add "one" "two")

;;Another is to give functions somewhere to write strings to:
(defn w-bind [[value s] function]
  (let [[nvalue newstring] (function value)]
    [nvalue (str s newstring)]))
(defn w-unit [value]
  [value ""])

(defn debug-add [a b]
  [(+ a b) (str "(+ " a " " b ")->" (+ a b) "; ")])
(defn debug-mul [a b]
  [(* a b) (str "(* " a " " b ")->" (* a b) "; ")])
(defn debug-sub [a b]
  [(- a b) (str "(- " a " " b ")->" (- a b) "; ")])

(defn debug-fib [n]
  (if (< n 2) 
    [n (str "(fib " n ") -> " n "; ")]
    (let [[fn string] (do-monad [w-bind w-unit]
                          [n1 (debug-sub n 1)
                           n2 (debug-sub n 2)
                           f1 (debug-fib n1)
                           f2 (debug-fib n2)
                           fn (debug-add f1 f2)]
                        fn)]
      [fn (str string "(fib " n ")-> " fn "; ")])))

(debug-fib 1)
(debug-fib 2)
(debug-fib 3)

(do-monad [w-bind w-unit]
    [a (debug-add 1 1)
     b (debug-add a 1)
     c (debug-mul a b)
     d (debug-fib c)]
  [a b c d])

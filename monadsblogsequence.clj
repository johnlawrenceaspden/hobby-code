;;The sequence monad

;;I'm currently reading the excellent tutorial on monads here: 
;;http://onclojure.com/2009/03/05/a-monad-tutorial-for-clojure-programmers-part-1/ 
;;and paraphrasing it to help me understand.

;;You may prefer to look at my earlier post 

;;http://learnclojure.blogspot.com/2009/09/how-it-works-monad-im-currently-reading.html
;;first. This is a follow-up.

;;We've already seen that 

((fn [a]
   ((fn [b]
      (* a b))
    2))
 1)

;;is the same as:
(let [a 1
      b 2]
  (* a b))

;;The functional for loop

(for [a (range 5)
      b (range a)]
  (* a b))

;;has a similar structure. 
;;Now the variables are being attached to members of sequences, and the earlier names can 
;;be used in the calculation of the later values. At the end, a sequence results.

;;If we didn't have for, what could we write to get the same effect?
;;The obvious analogous answer:
(map (fn [a]
       (map (fn [b]
              (* a b))
            (range a)))
     (range 5))
;;Doesn't quite work, because the results are nested. We actually need either:
(mapcat (fn [a]
       (map (fn [b]
              (* a b))
            (range a)))
     (range 5))

;;or 
(mapcat (fn [a]
       (mapcat (fn [b]
              (list (* a b)))
            (range a)))
     (range 5))
;;to reproduce the same result as for.

;;There is obviously something I don't understand here, because I prefer the first version, 
;;but the second version is the monadic way. Let us use that and see whether there's
;;a good reason later on.

;;Our new bind function is:
(defn s-bind [value function]
  (mapcat function value))
;;and we'll call the other function unit
(defn s-unit [value]
  (list value))

;;And now we can write:
(s-bind (range 5) (fn [a]
        (s-bind (range a) (fn [b]
                            (s-unit (* a b))))))

;;Let's tidy up the syntax with a macro again.
;;Because the for transformation is more complex, we need a more complex macro than before, 
(defmacro do-monad [[binder result] bindings expression]
  (if (= 0 (count bindings))
    `(~result ~expression)
    `(~binder ~(second bindings) (fn[~(first bindings)]
       (do-monad [~binder ~result] ~(drop 2 bindings) ~expression)))))

;;which allows us to write:
(do-monad [s-bind s-unit]
          [a (range 5)
           b (range a)]
          (* a b))
;;instead.

;;With our new monad, we've recreated the functional for loop
(for [a '(1 2 3) 
      b '(10 20 30) 
      c '(100 200 300)] 
  (+ a b c))

;;remember that we have the earlier names available lower down
(do-monad [s-bind s-unit]
          [end  (range 6)
           begin (range end)
           second (range (inc begin) end)
           third (range (inc second) end)]
          (list begin second third end))

;;This expression may be thought of as a loop, or as a sequence of multiple valued computations.
;;We're saying 'take all paths in'
;;
;;Choose end from (all numbers from zero but less than six)
;;Choose begin from (all numbers from zero but less than end)
;;Choose second from (all numbers between begin and end)
;;Choose third from (all numbers between second and end)
;;  give me the tuple (begin second third end)



;;We can instantly create another monad, using sets instead of lists
(defn set-bind [sequence function]
  (set (mapcat function sequence)))
(defn set-unit [value]
  (set (list value)))

;;It's effectively the same, but at every step it removes duplicates
(do-monad [set-bind set-unit]
          [a '(1 2 3)
           b '(1 2 3)
           c '(1 2 3)]
          (+ a b c))

;;There's a sense in which a monad is the two functions bind and unit.

;;Our earlier examples, the identity monad and the maybe monad, only seemed 
;;to have bind, but they fit into the monad framework if we take their unit function
;;to be the identity function (fn [x] x)

;;To recap:

;;Using the identity monad, or let, we can chain functions into arbitrary nets

;;Using the maybe monad, we can chain arbitrary functions which take values
;;but produce either values or nil.

;;Using the sequence monad, we can chain functions which take values and produce sequences
;;into arbitrary computational nets. 

;;We can think of using the set monad as chaining multiple valued functions.

;;So monads are to do with chaining computations, and with naming intermediates
;;They are generalizations of let and for, which are powerful concepts that we know and use
;;all the time.

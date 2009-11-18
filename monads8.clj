;;Monads are generalizations of let. They are to do with binding names to values.

;;Consider

(let [a 2
      b (inc a)]
  (* a b))

;;Which might also be written:

((fn [a]
   ((fn [b]
      (* a b))
    (inc a))) 
 2)

;;We have a problem reading the second definition, which we can tidy up using 

(defn bind [value function]
  (function value))

;;So now we can write

(bind 2       (fn [a]
(bind (inc a) (fn [b]
               (* a b)))))

;;This is more readable, since the values are now near their names.

(defmacro do-monad [binding-function binding-vector expression]
  (if (< (count binding-vector) 2)
  expression
  `(~binding-function ~(second binding-vector) (fn [~(first binding-vector)]
     (do-monad ~binding-function ~(drop 2 binding-vector) ~expression)))))


;;So now our monad is as easy to use as let.
(do-monad bind 
          [a 2 
           b (inc a)] 
          (* a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;What sort of problems can we use a generalization of let on?

(def database {:Aspden
               {:John "john@mailinator.com"
                :Lawrence "dad@mailinator.com"
                :Marylin "mum@mailinator.com"}
               :Phelan
               {:Donald "don@mailinator.com"
                :Elsie  "elsie@mailinator.com"}})


(defn address-length [forename surname]
  (let [family (database surname)
        email  (family forename)]
    (count email)))


(address-length :Lawrence :Aspden) ;fine
(address-length :Elizabeth :Aspden) ;gives 0, but I think it should produce nil because I forgot to put Sis in the database
(address-length :Neil :Satterthwaite) ;throws an exception because Neil's family's not there.


;; Looking Neil up causes an exception, because his family's not in the database, 
;; so the nil return value causes trouble. We need to catch the nils:

(defn address-length [forename surname]
  (let [family (database surname)]
    (if (nil? family) nil
        (let [email (family forename)]
          (if (nil? email) nil
              (count email))))))

;;And then we get the nil return value that we'd get if we looked up 

;;Alternatively we could use our monadic version of let, but with a different bind function 

(defn nil-bind [value function]
  (if (nil? value) nil (function value)))

(defn address-length [forename surname]
  (do-monad nil-bind
            [family (database surname)
             email (family forename)]
            (count email)))

(address-length :Lawrence :Aspden) 18
(address-length :Elizabeth :Aspden) nil
(address-length :Neil :Satterthwaite) nil

;;What's going on? Well, the do-monad with the new bind looks like:
(macroexpand '(do-monad nil-bind
            [family (database surname)
             email (family forename)]
            (count email)))

(nil-bind (database surname) (fn [family] 
    (nil-bind (family forename) (fn [email] 
                                (count email)))))

;;Which means that if either intermediate lookup returns nil, the 
;;whole expression short circuits and returns nil. 
;;Only if both (database surname) and (family forename) are non-null do we call the count function.


;;Although this example is fairly trivial, it should show that there are cases where a let-like
;;construct is useful to avoid copy-and-paste coding.

;;Both examples are built in to clojure, as the identity and maybe monads.

;;They can be used thus:

(use 'clojure.contrib.monads)

(domonad identity-m
         [a 1
          b (inc a)]
         (* a b))

(domonad maybe-m
         [us (database :Aspden)
          email (us :Liz)]
         (count email))
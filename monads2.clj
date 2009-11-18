;;Monads are about composing computational steps into a bigger computation

;;Some monads are built in to clojure
;;The thing known as let is also called the identity monad

;;Consider

(let [a 2
      b (inc a)]
  (* a b))

;;One could also write

(let [a 2]
  (let [b (inc a)]
    (* a b)))

;;Let is just a thin skin over fn:
((fn [a] (inc a)) 2) 
;;is entirely equivalent to
(let [a 2] (inc a))

;;So one could also write

((fn [a]
   (let [b (inc a)]
     (* a b))) 
 2 )
  
;;and doing the same thing with the second let
((fn [a]
   ((fn [b] 
      (* a b)) 
    (inc a)))
 2)
;;So if clojure hadn't defined let, we'd be able to get along without it by using fns, 
;;at the cost of a certain unreadability.

;;Some of the unreadability comes from the argument names being away from their values
;;That can be mitigated so:

(defn bind [value function]
  (function value))

;;So now we can write:
(bind 2 (fn [a]
   ((fn [b] 
      (* a b)) 
    (inc a))))

;;or even better
(bind 2       (fn [a]
(bind (inc a) (fn [b] 
   (* a b)))))

;;And we could even tidy that up with a macro, 
(defmacro do-let-monad
   [steps expr]
   (let [rsteps (reverse (partition 2 steps))
	 [lr ls] (first rsteps)]
       (reduce add-monad-step
	       expr
	       rsteps)))

(defn add-monad-step
  [mexpr step]
  (let [[bform expr] step]
    (list 'bind expr (list 'fn [bform] mexpr))))

;;which would allow us to write something like:
(do-let-monad
  [ a 2 
    b (inc a) ]
 (* a b))

(macroexpand-1 '(do-let-monad
  [ a 2 
    b (inc a) ]
 (* a b)))

(bind 2 (fn [a] (bind (inc a) (fn [b] (* a b)))))

;;I don't have to write that macro, because clojure provides it

(use 'clojure.contrib.monads)

(domonad identity-m
          [ a 2
            b (inc a) ]
          (* a b))

;;So we know now that at least one monad, the identity monad, is the same as something that 
;;we find very useful, the let form. And we know that using the identity monad is a lot
;;easier than manually composing the computations that we'd have to use if let wasn't a built in.
;;Another way of saying that is that they are a sort of generalization of the macro that we'd use
;;to implement let if we didn't already have it.


;;Exercises:

;1 Implement the fibonnacci function using let, using fn, and using the identity monad

(defn fib [n]
  (if (< n 2) n
      (+ (fib (- n 2)) (fib (- n 1)))))

(map fib (range 10)) ;-> (0 1 1 2 3 5 8 13 21 34)

(defn fibl [n]
  (if (< n 2) n
      (let [f2 (fibl (- n 2))
            f1 (fibl (- n 1))]
        (+ f2 f1))))

(map fibl (range 10))

(defn fibf [n]
  (if (< n 2) n
      ((fn [f2] 
         ((fn [f1] 
            (+ f2 f1))
          (fibf (- n 1))))
        (fibf (- n 2)))))

(map fibf (range 10))

(defn fibm [n]
  (if (< n 2) n
      (domonad identity-m
                [f2 (fibm (- n 2))
                 f1 (fibm (- n 1))]
                (+ f1 f2))))

(map fibm (range 10))

;;Exercise 2. Take the monadic fib to its logical conclusion 

(defn fibmm [n]
  (if (< n 2) n
      (domonad identity-m
               [n1 (dec n) 
                n2 (dec n1)
                f1 (fibmm n1)
                f2 (fibmm n2)]
               (+ f1 f2))))

(map fibmm (range 10))
;;How it Works: the Monad

;;I'm currently reading the excellent tutorial on monads here: 
;;http://onclojure.com/2009/03/05/a-monad-tutorial-for-clojure-programmers-part-1/
;;and paraphrasing it to help me understand. 

;;The simplest monad is let, the identity monad.

(let [a 1]
  (let [b 2]
    (let [c (* a b)]
      (let [d (* a a)]
        (let [e (+ (* b b) (/ c d))]
          (let [f (+ c d e)]
            (let [g (- c)]
              (* a b c d e f g))))))))


;;The let above represents a complex computation.

;;During the computation, values are computed, names are bound to values, values are
;;used as inputs to functions.

;;Values could also be pulled in from the global namespace, and side effects
;;could be produced. Let is a very simple thing allowing the representation of
;;very complex and powerful things.

;;In fact, let is only a syntactic variant of lambda, or as clojure calls it, fn.

((fn[a] (*a 2)) 1)

;;is exactly equivalent to 

(let [a 1]
  (* a 2))

;;In fact in some lisps, that's how it's implemented.

;;If all we had was fn, we could build let with a simple macro.
;;The reverse is also true.

(let [a 1]
  (let [b (inc a)]
    (* a b)))

;;is just

((fn [a]
   ((fn [b]
      (* a b)
      ) (inc a))
   ) 1)

;;And it's easy to transform one into the other.

;;But the second form is much harder to read.

;;If we didn't have let, how could we do this sort of thing and remain sane?

;;We could make it easier to read by defining the function bind:

(defn bind [value function]
  (function value))

;;This, by reversing the order of function and argument, allows us to write:
(bind 1 (fn [a]
          (bind (inc a) (fn [b]
                          (* a b)))))

;;Thus putting the names nearer to the values they take.

;;And then, this being lisp, we could introduce a special syntax to take away the
;;boilerplate:

(defmacro with-binder [binder bindings expression]
  (if (= 0 (count bindings))
    expression
    `(~binder ~(second bindings) (fn[~(first bindings)]
                                (with-binder ~binder ~(drop 2 bindings) ~expression)))))


;;and we have let back!
(with-binder bind 
  [a 1
   b (inc a)]
  (* a b))

;;Notice that I've put bind as a parameter of the macro. It could have been hard coded,
;;but now we can put other functions in its place.

;;There are sometimes reasons to use a different bind.

;;For instance, suppose that we have functions that can produce nil.

;;Consider a function which looks something up in a list and returns nil if it's not there.
(defn string->int[x]
  (condp = x "one" 1 "two" 2 nil))
(defn int->string[x]
  (condp = x 1 "one" 2 "two" nil))

;;Here are some unit tests:
(map string->int '("one" "two" "three"))
(map int->string '(1 2 3))

;;If we want to compute without throwing exceptions, we need to catch nils
;;and short-circuit the bits of the computation that can't deal with them.
(defn buggy-add-string [a b]
  (let [av (string->int a)
        bv (string->int b)
        cv (+ av bv)
        c (int->string cv)] 
          c))

(buggy-add-string "one" "one")   ;;-> "two"
(buggy-add-string "one" "two")   ;;-> nil
(buggy-add-string "one" "three") ;;is an error. 

;;We need instead to write:
(defn guarded-add-string [a b]
  (let [av (string->int a)]
    (if (nil? av) nil
        (let [bv (string->int b)]
          (if (nil? bv) nil
              (let [cv (+ av bv)]
                (let [c (int->string cv)]
                  c)))))))

(guarded-add-string "one" "one")   ;;-> "two"
(guarded-add-string "one" "two")   ;;-> nil
(guarded-add-string "one" "three") ;;-> nil 

;;This sort of code is repetitive, difficult to read and understand, boring and error-prone to write.
;;If we want to do this sort of thing (and we want to do it all the time!) we need to find 
;;a way of abstracting the pattern away, so that we can leave the 
;;interesting parts to be our program.

;;First let's notice that it doesn't do any harm to check for nils even after functions
;;that don't produce them, so we could make the code more uniform by always checking:

(defn over-guarded-add-string [a b]
  (let [av (string->int a)]
    (if (nil? av) nil
        (let [bv (string->int b)]
          (if (nil? bv) nil
              (let [cv (+ av bv)]
                (if (nil? cv) nil
                    (let [c (int->string cv)]
                      (if (nil? c) nil
                          c)))))))))

;;Of course, this makes the readability even worse, but we have more hope of 
;;abstracting away a uniform pattern.

;;By analogy with the bind function above, we can use maybe-bind to abstract away the 
;;checking.

(defn maybe-bind [value function]
  (if (nil? value) nil
      (function value)))

(defn maybe-add-string [a b]
  (maybe-bind (string->int a) (fn [av]
        (maybe-bind (string->int b) (fn [bv]
              (maybe-bind (+ av bv) (fn [cv]
                    (maybe-bind (int->string cv) (fn[c]
                          c)))))))))

(maybe-add-string "one" "one")   ;;-> "two"
(maybe-add-string "one" "two")   ;;-> nil
(maybe-add-string "one" "three") ;;-> nil

;;This is still a bit of a nightmare, but a similar pattern to the one above has emerged.

;;If we use our macro from above, it looks much better
(defn monadic-add-string [a b]
  (with-binder maybe-bind
    [av (string->int a)
     bv (string->int b)
     cv (+ av bv)
     c (int->string cv)] 
    c))

;;but it still works:
(monadic-add-string "one" "one") ;;-> "two"
(monadic-add-string "one" "two") ;;-> nil
(monadic-add-string "one" "three") ;;-> nil


;;This is just like the code we would have written if we'd been happy to let
;;nils cause exceptions. 

;;We've literally substituted "with-binder maybe-bind" for "let"
;;and all the horror has gone away. 

;;I think this would be pretty impressive of itself, since removing exactly 
;;this source of complexity was the major motivation for inventing exception handling.

;;But it turns out to be an almost trivial example of a very general pattern where you want to 
;;process values before assigning them to variables in a series of lets.


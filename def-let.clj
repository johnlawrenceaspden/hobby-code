;; On Narayan Singhal's blog, I found a truly astonishing macro:

;; Here's a link to Narayan's original post:
;; http://clojure101.blogspot.com/2009/04/destructuring-binding-support-in-def.html

;; The idea of it is that let statements are often difficult to debug.

;; Suppose you are constructing a calculation bit by bit.

;; You will, I hope, forgive the arbitrary and trivial nature of the following
;; example.  I needed a complicated example that is nevertheless easy to
;; understand so that it won't distract from the genius of the idea.

;; You might originally say, take the range from 0 to 9 and map the function
;; x -> 10x over it, to produce a lookup table:
(zipmap (range 10) (map #(* 10 %) (range 10)))

;; And then do the same for the functions x -> 5x^2 and x -> x^3
(zipmap (range 10) (map #(* 5 % %) (range 10)))
(zipmap (range 10) (map #(* % % %) (range 10)))

;; Now say, to construct a map from (range 10) to the minimum of these three functions
(merge-with min
            (zipmap (range 10) (map #(* 10 %) (range 10)))
            (zipmap (range 10) (map #(* 5 % %) (range 10)))
            (zipmap (range 10) (map #(* % % %) (range 10))))

;; And for the maximum, you might say:
(merge-with max
            (zipmap (range 10) (map #(* 10 %) (range 10)))
            (zipmap (range 10) (map #(* 5 % %) (range 10)))
            (zipmap (range 10) (map #(* % % %) (range 10))))

;; Now the thing is, that expressions such as these are very easy to debug.  In
;; emacs, for instance, you'd just put your cursor after a subexpression and use
;; C-x e to evaluate it.

;; Since the subexpressions are all valid code, you can just watch the result
;; building up by looking at the various subexpressions.

(range 10) ;; (0 1 2 3 4 5 6 7 8 9)
(map #(* 5 % %) (range 0 9)) ;;(0 5 20 45 80 125 180 245 320)
(zipmap (range 0 9) (map #(* 5 % %) (range 0 9)))
;; {8 320, 7 245, 6 180, 5 125, 4 80, 3 45, 2 20, 1 5, 0 0}
(map #(* % % %) (range 0 9)) ;; (0 1 8 27 64 125 216 343 512)
;; and so on, all the way to one of the final results:

(merge-with min
            (zipmap (range 10) (map #(* 10 %)  (range 10)))
            (zipmap (range 10) (map #(* 5 % %) (range 10)))
            (zipmap (range 10) (map #(* % % %) (range 10))))
;; {0 0, 1 1, 2 8, 3 27, 4 40, 5 50, 6 60, 7 70, 8 80, 9 90}

;; If you have a lisp-aware editor that can evaluate arbitrary sub-expressions
;; in a file, then such code is very easy to read and understand.

;; But of course, although you might well construct the final list in exactly
;; that way while experimenting at the REPL or writing the program in the first
;; place, you wouldn't use such expressions in your final program.

;; I think I'd feel a certain icy contempt if I came upon this expression in a
;; finished program:

(list
 (merge-with min
             (zipmap (range 10) (map #(* 10 %)  (range 10)))
             (zipmap (range 10) (map #(* 5 % %) (range 10)))
             (zipmap (range 10) (map #(* % % %) (range 10))))

 (merge-with max
             (zipmap (range 10) (map #(* 10 %)  (range 10)))
             (zipmap (range 10) (map #(* 5 % %) (range 10)))
             (zipmap (range 10) (map #(* % % %) (range 10)))))



;; Easy to work through bit by bit though it is, there's just an awful lot of
;; redundancy.  Imagine trying to find a bug caused by an 11 having sneaked in
;; there somehow. There are twelve different places where (range 10) is used.

;; Any programmer who would write such code is neglecting the cardinal virtue of
;; laziness, or, as it is sometimes known, the principle of not repeating
;; yourself twice .

;; Much more idiomatic, easier to understand just by reading, less redundant,
;; more easily modifiable, and altogether better style would be:

(let [r (range 10)
      make-map   (fn [f] (zipmap r (map f r)))
      linears    (make-map #(* 10 %))
      squares    (make-map #(* 5 % %))
      cubes      (make-map #(* % % %))
      mins       (merge-with min squares cubes linears)
      maxs       (merge-with max squares cubes linears)]
  (list mins maxs))

;; This is my preferred version. It seems clear but not excessively redundant.

;; However, if you were very keen on terseness, you might say:

(let [r (range 10)
      make-map   (fn [f] (zipmap r (map f r)))
      [linears, squares, cubes] (map make-map (list  #(* 10 %) , #(* 5 % %) ,  #(* % % %)))
      merge      (fn[f] (merge-with f linears squares cubes))
      mins       (merge min)
      maxs       (merge max)]
  (list mins maxs))

;; or perhaps even:

(let [r (range 10)
      make-map   (fn [f] (zipmap r (map f r)))
      fns        (map make-map (list  #(* 10 %) #(* 5 % %) #(* % % %)))
      merge-fns  (fn[f] (apply merge-with f fns))]
  (map merge-fns (list min max)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So far so good, but what about when you come to read your code 6 months
;; later?

;; You might be able to work out what the fourth version does in your head, but
;; it would be work.

;; I tend to find myself working out how complex let statements work by doing
;; this sort of thing in a little corner of the file:

(def r         (range 10))
(def make-map  (fn [f] (zipmap r (map f r))))
(def fns       (map make-map (list  #(* % % %) #(* 5 % %) #(* 10 %))))
(def merge-fns (fn[f] (apply merge-with f fns)))
(map merge-fns (list min max))

;; Now I am back to being able to evaluate the intermediate results.  I can just
;; move down the list of top level expressions evaluating interesting-looking
;; bits with C-x e and defining them as globals with M-C-x until I can see how
;; the whole thing fits together.

;; Narayan obviously does the same sort of thing from time to time, and his
;; insight is that this is a rather mechanical process.

;; Surely there's some way in which your environment or compiler can help?

(defmacro def-let
  "like let, but binds the expressions globally."
  [bindings & more]
  (let [let-expr (macroexpand `(let ~bindings))
        names-values (partition 2 (second let-expr))
        defs   (map #(cons 'def %) names-values)]
    (concat (list 'do) defs more)))

;; This is actually not Narayan's original macro.  I have modified it (notably
;; so that if there's an error somewhere in the let, it will get some of the way
;; down so that you can see why it broke) and simplified it a little, but the
;; idea is his, and any errors I have introduced are my fault.

;; Here's our let statement:

(let [r (range 10)
      make-map   (fn [f] (zipmap r (map f r)))
      fns        (map make-map (list  #(* % % %) #(* 5 % %) #(* 10 %)))
      merge-fns  (fn[f] (apply merge-with f fns))]
  (map merge-fns (list min max)))

;; Change let to def-let:

(def-let [r (range 10)
      make-map   (fn [f] (zipmap r (map f r)))
      fns        (map make-map (list  #(* % % %) #(* 5 % %) #(* 10 %)))
      merge-fns  (fn[f] (apply merge-with f fns))]
  (map merge-fns (list min max)))

;; Evaluate it. The actual result is the same, but the intermediate expressions
;; have been defined as globals.

(list r make-map fns merge-fns)
((0 1 2 3 4 5 6 7 8 9)
 #<user$make_map user$make_map@1a15b82>
 ({0 0, 1 1, 2 8, 3 27, 4 64, 5 125, 6 216, 7 343, 8 512, 9 729}
  {0 0, 1 5, 2 20, 3 45, 4 80, 5 125, 6 180, 7 245, 8 320, 9 405}
  {0 0, 1 10, 2 20, 3 30, 4 40, 5 50, 6 60, 7 70, 8 80, 9 90})
 #<user$merge_fns user$merge_fns@26920c>)

;; This not only means that you can look at them, it means that you can now take
;; interesting sub-expressions in the let statement and evaluate them, and this
;; will work, because the missing values are provided by the globals!!

;; e.g.

(make-map #(* % % %))
;;{0 0, 1 1, 2 8, 3 27, 4 64, 5 125, 6 216, 7 343, 8 512, 9 729}

;; Now of course this has the same disadvantage as the scratchpad method, in
;; that it sprays values all over your namespace.

;; But we're only debugging here. Style doesn't count, and I was doing the
;; scratchpad thing anyway. Now I can screw up faster!

;; So I think I'm going to be using def-let a lot.

;; Even better, it can deal with destructuring in the same manner as a let
;; statement can:

;; Here's a broken let statement. It won't execute at all. How would you debug
;; it if it wasn't so trivial?

(let [a 1
      b 2
      [c d] [(+ a b) (- a b)]
      e (c)]
  (list a b c d e))

;; Let's try def-let:

(def-let [a 1
          b 2
          [c d] [(+ a b) (- a b)]
          e (c)]
  (list a b c d e))

;; Same error, but it's done what it could:
a ;1
b ;2
c ;3
d ;-1
e ; var user/e is unbound.

;; So we can see that the problem was in the last bit, the assignment to e,
;; where we're trying to call c, even though it's a number not a function.

;; In fact def-let created more variables than that:

(macroexpand '(def-let [a 1
                        b 2
                        [c d] [(+ a b) (- a b)]
                        e (c)]
                (list a b c d e)))

(do (def a 1)
    (def b 2)
    (def vec__11853 [(+ a b) (- a b)])
    (def c (clojure.core/nth vec__11853 0 nil))
    (def d (clojure.core/nth vec__11853 1 nil))
    (def e (c)) (list a b c d e))

;; So in fact we can see how the destructuring binds really work.  This isn't in
;; fact that useful as far as I can tell, because the gensym when we macroexpand
;; the expression isn't the same one as when we evaluate it, but it might lead
;; to some sort of insight into what's going on.

;; And it works for all the destructuring forms that let does.

(let [a {:a 1 :b 2}
      {:keys [a b]} a]
  [a b])

;;[1 2]

(macroexpand '(def-let [a {:a 1 :b 2}
                        {:keys [a b]} a]
                [a b]))

(do (def a {:a 1, :b 2})
    (def map__11926 a)
    (def map__11926 (if (clojure.core/seq? map__11926)
                      (clojure.core/apply clojure.core/hash-map map__11926)
                      map__11926))
    (def b (clojure.core/get map__11926 :b))
    (def a (clojure.core/get map__11926 :a))
    [a b])

;; I've never seen anything like this before, and yet it strikes me as a useful
;; debugging technique, and it's been staring us in the face for years. If ever
;; you find yourself doing something repetitive and mechanical, macros can make
;; it go away.

;; With a little extra work, it would be possible to get def-let to produce an
;; extra definition of a function, say remove-crapspray or something, to undo
;; the damage that it's done once you've finished.

;; Obviously this is just crazy talk. But I'm going to do it anyway. If anyone's
;; interested in the final version, let me know.


















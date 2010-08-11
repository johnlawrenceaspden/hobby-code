;; Reduce is sometimes thought to be somewhat mysterious.

;; Often one needs to loop over a collection, and store results in an accumulator.

;; Three very fundamental operators in functional programming are map, filter and reduce

;; Most people think that map and filter are fairly obvious, but there seems to be a certain amount 
;; of confusion about reduce.

;; But it's actually very simple in concept, and represents an easy idea.


;; The simplest example would be adding the numbers in a list.

;; Suppose the numbers are 0,1,2,3,4,5,6,7,8,9 and we want to find their sum

;; In C, or another naturally imperative language, we'd say:

;; int list[]={1,2,3,4,5,6,7,8,9,10}; //or something.
;; int len=10;

;; int a=0;
;; int i;

;; for (i=0; i<len; i++){
;;      a += list[i];
;; }

;; It's been a while since I've used C,  and I can't remember the syntax!
;; But I'm pretty sure that this is idiomatic code, if it's code at all.

;; In fact, in C, for is a syntactic abstraction, and the loop becomes

;; i=0;
;; while(i<len)
;; {
;;   a += list[i];
;;   i++;
;; }

;; This seems a perfectly natural and straightforward way to do things, and it pisses me off 
;; that you can't write this directly in languages with functional pretensions. Let's see how close we can get:
;; Clojure means something else by for, so let's translate the while loop version:
;; Using atoms to provide mutable state, we can do something similar

(def lst '(1 2 3 4 5 6 7 8 9 10))
(def len 10)

(def a (atom 0))
(def i (atom 0))

(loop []
    (when (< @i len)
      (swap! a + (nth lst @i))
      (swap! i inc)
      (recur)))

;; The value ends up in the atom a, just as in the C version.

;; In clojure, this looks more complicated, 
;; partly because we've got to build the loop by hand, from loop, when and recur

;; partly because mutation in clojure is intrinsically more complicated, because
;; clojure is extremely concerned with thread safety, and so we need to allocate 
;; and dereference atoms rather than mutating local variables.

;; and partly because C has very good notations for its fundamental operations. 

;; But logically they're the same algorithm.


;; However, I feel dirty just writing this code in clojure, even
;; though that would have been a perfectly good piece of LISP in the sixties.

;; The function is unnecessarily complicated in clojure because the idea 
;; of mutating the value of a variable is foreign to clojure. 
;; You can do it, but it's hard work.
 
;; A more natural way to do it in clojure is the loop-as-function-call:

(loop [a 0 i 0]
  (if (= i len) a
  (recur (+ a (nth lst i)) (inc i))))

;; This is much more idiomatic code in clojure, and it doesn't mutate any values
;; even though the variable-rebinding in the recur call produces a very similar
;; effect.

;; And here the final value is the value of the expression, which is nicer.

;; Also of course, clojure's lists know how long they are, or at least when 
;; they are empty or not, so we don't need an explicit loop counter either.

;; So how about
(loop [a 0 l lst]
  (if (empty? l) a
      (recur (+ a (first l)) (rest l))))

;; l moves along the list, while a accumulates the values.

;; We can imagine that this is a common pattern
(loop [acc _ l _]
  (if (empty? l) a
      (recur (_ a (first l)) (rest l))))

;; almost as common as
;; a= _ 
;; for(i=0; i<_; i++)
;; { 
;;   a _= _ [i] 
;; } 
;; is in C

;; Where in both cases we need to fill in the _ with the initial value of 
;; the accumulator, the list to be accumulated over, 
;; and the operation to be performed.

;; And indeed it is. The pattern is called reduce, and it works on anything
;; that naturally has a number of items in it

(reduce + 1 lst)

;; reduce is clojure's natural way of expressing accumulation over a list
;; in the same way as the for-loop over += and ++ is C's

;;Here are some examples

(reduce * 1 lst) ;; use * instead of +, and start with 1
(reduce conj '() lst)


(reduce * #{1,2,3})
(reduce * [1,2,3])

(reduce (fn[[a s] b] [(+ a b), (str s b)]) [0,""] lst)
;; In these cases where the function is commutative and associative, we might think of reduce as meaning:

(reduce + 0 '(1 2 3 4 5 6 7 8 9 10)) -> (0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10)
(reduce * 1 '(1 2 3 4 5 6 7 8 9 10)) -> (1 * 1 * 2 * 3 * 4 * 5 * ........)

(reduce conj '() '(1 2 3))

((fn [m w] (assoc m w (inc (m w 0)))) {} "fred")

(sort #(< (%1 1) (%2 1)) 
      (reduce 
       (fn [m w] (assoc m w (inc (m w 0)))) 
       {} 
       (clojure.contrib.string/split #"\W" 
                                     (slurp "/home/john/hobby-code/reduce.clj"))))


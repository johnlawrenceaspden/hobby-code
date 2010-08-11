;; Reduce is sometimes thought to be somewhat mysterious.

;; Often one needs to loop over a collection, and store results in an accumulator.

;; Three very fundamental operators in functional programming are map, filter and reduce

;; Most people think that map and filter are fairly obvious, but there seems to be a certain amount 
;; of confusion about reduce.

;; But it's actually very simple in concept, and represents an easy idea.


;; The simplest example would be adding the numbers in a list.

;; Suppose the numbers are 0,1,2,3,4,5,6,7,8,9 and we want to find their sum

;; In C, or another naturally imperative language, we'd say:

int list[]={0,1,2,3,4,5,6,7,8,9}; //or something.
int len=10;

int a=0;
int i;

for (i=0; i<len; i++){
     a=a+list[i];
}

;; It's been a while since I've used C,  and I can't remember the syntax!
;; But I'm pretty sure that this is idiomatic code, if it's code at all.

;; This seems a perfectly natural and straightforward way to do things, and it pisses me off 
;; that you can't write this directly in languages with functional pretensions. Let's see how close we can get:

(def lst '(0 1 2 3 4 5 6 7 8 9))
(def len 10)

;; Using atoms to provide mutable state, we can do something similar

(def a (atom 0))
(def i (atom 0))

(loop []
    (when (< @i len)
      (swap! a + (nth lst @i))
      (swap! i inc)
      (recur)))

;; The value ends up in the atom a, just as in the C version.

;; In clojure, this looks more complicated, partly because we've got to build the loop by hand, 
;; partly because mutation in clojure is intrinsically more complicated, and partly because C has a very
;; good notation for the things that it's good at. But logically they're the same algorithm

;; However, I feel dirty just writing this code in clojure.

;; The function is unnecessarily complicated in clojure because the idea of mutating the value of 
;; a variable is foreign to clojure. You can do it, but it's hard work.
 
;; There's no need to mutate state just to stick the contents of a data structure in an accumulator.

;; We can do almost the same thing clojure without using mutation:

(loop [a 0 i 0]
  (if (= i len) a
  (recur (+ a (nth lst i)) (inc i))))

;; And here the final value is the value of the expression, which is nicer

;; Also of course, clojure's lists know how long they are, or at least when they are empty or not:

;; So how about
(loop [a 0 l lst]
  (if (empty? l) a
      (recur (+ a (first l)) (rest l))))

;; We can imagine that this is a common pattern

(loop [acc _ l _]
  (if (empty? l) a
      (recur (_ a (first l)) (rest l))))

;; Where we need to fill in the _ with the initial value of the accumulator, the list to be accumulated over, 
;; and the operation to be performed.

;; And indeed it is. The pattern is called reduce.

(reduce + 0 lst)

;; In this case where the function is commutative and associative, we might think of reduce as meaning:

(reduce + 0 '(1 2 3 4 5 6 7 8 9 10)) -> (0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10)







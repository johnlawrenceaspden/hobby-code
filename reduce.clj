;; Three very fundamental operators in any sort of programming are map, filter
;; and reduce.

;; They represent the common programming tasks of transforming a collection,
;; selecting from it, and summing over it.

;; Most people think that map and filter are fairly obvious, but there seems to
;; be a certain amount of confusion about reduce.

;; But it's actually very simple in concept, and represents an easy idea.

;; Often one needs to loop over a collection, and store results in an
;; accumulator.

;; The simplest example of a reduction would be adding the numbers in a list.

;; Suppose the numbers are 1,2,3,4,5,6,7,8,9,10 and we want to find their sum

;; In C, or another naturally imperative language, we'd say:

;; int list[]={1,2,3,4,5,6,7,8,9,10};
;; int len=10;

;; int a=0;
;; int i;

;; for (i=0; i<len; i++){
;;      a += list[i];
;; }

;; Using atoms to provide mutable state, we can do something similar

(def lst '(1 2 3 4 5 6 7 8 9 10))
(def len 10)

(def a (atom 0))

(dotimes [i len]
      (swap! a + (nth lst i)))

;; The value ends up in the atom a, just as in the C version.

;; In clojure, this looks more complicated.

;; Partly because mutation in clojure is intrinsically more complicated, because
;; clojure is extremely concerned with thread safety, and so we need to allocate 
;; and dereference atoms rather than mutating local variables.

;; And partly because C has very good notations for its fundamental operations,

;; But logically they're the same algorithm.

;; But I'd feel dirty writing this code in clojure, even though that would have
;; been a perfectly good piece of LISP in the sixties. It's just a feeling that
;; I have that it is better to avoid mutation unless it's actually necessary.
;; Even though the mutation-less algorithms are often harder to write, they're
;; often easier to debug and test.

;; A more natural way to accumulate over a list in clojure is the 
;; loop-as-function-call, with accumulator and iterator as parameters:

(loop [a 0 i 0]
  (if (= i len) a
  (recur (+ a (nth lst i)) (inc i))))

;; This is much more idiomatic code in clojure, and it doesn't mutate any values
;; even though the variable-rebinding in the recur call produces a very similar
;; effect.

;; And here the final value is the value of the expression, which is nicer.

;; Of course, clojure's lists know when they are empty, so we don't need an
;; explicit loop counter.

;; So how about:
(loop [a 0 l lst]
  (if (empty? l) a
      (recur (+ a (first l)) (rest l))))

;; l moves along the list, while a accumulates the values.

;; It still looks a bit long-winded, but we can easily imagine that this is a
;; common pattern:
(loop [acc _ l _]
  (if (empty? l) a
      (recur (_ a (first l)) (rest l))))

;; Where the blanks represent holes in the boilerplate we have to fill in.

;; It should be almost as common as the equivalent pattern:

;; a= _ 
;; for(i=0; i<_; i++)
;; { 
;;   a _= _ [i] 
;; } 

;; is in C

;; Where in both cases we need to fill in the _ with the initial value of the
;; accumulator, the list to be accumulated over, and the operation to be
;; performed.

;; Pretty much the first law of programming is:
;; If you see a common pattern, you should name it and abstract it so it goes away.

;; The pattern is called reduce. 

;; We need to fill in the blanks with the function to do the accumulating,
;; the initial value of the accumulator, and the list

;; Since we're reducing the list lst, using the operation +, and starting 
;; with the value zero, we write:

(reduce + 0 lst)

;; reduce is clojure's natural way of expressing accumulation over a list
;; in the same way as the for-loop over += and ++ is C's

;; Here are some other examples

(reduce * 1 lst) 

;; We use * instead of +, and start with 1 instead of 0
;; This produces the product of the numbers in the list.

;; In these cases where the order of the arguments doesn't matter
;; we can think of reduce as 'put the function between the values'

(reduce + 0 '(1 2 3 4 5 6 7 8 9 10)) ; (0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10)
(reduce * 1 '(1 2 3 4 5 6 7 8 9 10)) ; (1 * 1 * 2 * 3 * 4 * 5 * ........)

;; But this image is actually harmful when the ordering does matter.
;; What's really going on is that the operator is being used to feed values
;; into the accumulator one by one.

(reduce + 0 '(1 2 3))   
;; proceeds like:
;; (+ 0 1) -> 1
;;         (+ 1 2) -> 3
;;                 (+ 3 3) -> 6

;; If this seems confusing, even though you're happy with the C version,
;; think about the C loop.

;; They're actually just different ways of expressing the same idea,
;; and should look equally natural. Seeing how either one can always be 
;; transformed into the other will help.

;; Here's an example where the order does matter:
(reduce conj '() '(1 2 3))

;; How do we think about this?
(conj '()  1)   ; -> '(1)
               (conj '(1) 2)   ; -> '(2 1)
                              (conj '(2 1) 3) ; -> '(3 2 1)

;; So 
(reduce conj '() '(1 2 3)) -> '(3 2 1)

;; Of course this simple reduction is so common that the pattern 
;; (reduce conj '() _ ) already has a name

(reverse '( 1 2 3 ))

;; Here's the definition of reverse in the clojure.core source code!
(defn reverse
  "Returns a seq of the items in coll in reverse order. Not lazy."
  {:added "1.0"}
  [coll]
    (reduce conj () coll))


;; An acceptable definition of reduce itself would be:

(defn my-reduce [fn init coll]
  (loop [acc init l (seq coll)]
    (if (empty? l) acc
        (recur (fn acc (first l)) (rest l)))))

;; This works on any collection
(my-reduce * 1 '(1 2 3)) ;; a list
(my-reduce * 1 #{1,2,3}) ;; a set
(my-reduce * 1  [1,2,3]) ;; a vector

;; The real reduce in clojure.core is an optimised version and can deal with all
;; sorts of collections efficiently, but in spirit it is just making every
;; collection into a sequence and then doing what my little skeleton above did.

;; In fact the library reduce also has another feature, which is that if you
;; don't provide an initial value for the accumulator, then it takes the first
;; element of the sequence as its initial value, and accumulates over the rest
;; of the sequence.

;; For operations which produce answers of the same type as their arguments,
;; this is often what you want.

(reduce * '(1 2 3 4)) ;24 
(reduce +  [1 2 3 4])  ;10
(reduce bit-xor '(1 2 3 4 5)) ;1

;; So why has this simple operation got a scary reputation?

;; I think it's because all the common cases are so useful that they have
;; already been abstracted away, like reverse. So in fact you don't meet it that
;; often in practice.

;; Let's see if we can construct something more interesting:

;; Suppose you had a list of strings

(def strlist '("fred" "barney" "fred" "wilma"))

;; And wanted to count how many times each string occurs in the list.

;; We want an accumulator to keep the strings and counts in, and a function
;; which will take that accumulator, and a new string, and return the updated
;; accumulator.

;; The obvious accumulator is a map. We'd want the answer to be something like
{"fred" 2, "barney" 1, "wilma" 1}

;; So what function will add strings to a map?  

;;In a rather naive and long-winded way:

(defn addtomap [map string]
  (let [oldval
        (if (contains? map string) 
          (map string) 
          0)]
        (assoc map string (inc oldval))))

;; Here's how we'd use it to count our list, starting from the empty map {}:
(addtomap {} "fred")                      ;; {"fred" 1}
(addtomap {"fred" 1} "barney")            ;; {"barney" 1, "fred" 1}
(addtomap {"fred" 1, "barney" 1} "fred")  ;; {"fred" 2, "barney" 1}
(addtomap {"fred" 2, "barney" 1} "wilma") ;; {"wilma" 1, "fred" 2, "barney" 1}

;; So the reduce part is obvious, once you have addtomap

(reduce addtomap {} strlist)

;; But a real Clojure programmer would look at addtomap and think:

;; We can write (map string 0) instead of 
;; (if (contains? map string) 
;;           (map string) 
;;           0)

;; So a better version of addtomap would be:

(defn addtomap [map string]
  (let [oldval (map string 0)]
        (assoc map string (inc oldval))))

;; And now the let statement looks redundant, so let's say
(defn addtomap [map string]
  (assoc map string (inc (map string 0))))

;; And then he might say 
;; "since I'm only going to use this function here, why not make it anonymous?"

;; And now the reduce looks like:
(reduce (fn [map string] (assoc map string (inc (map string 0)))) {} strlist)

;; And well, at this point, any reasonable man is going to think:
;; "Since I'm writing a one-liner, I might as well use the anonymous shorthand"

(reduce #(assoc %1 %2 (inc (%1 %2 0))) {} strlist)

;; And if you already understand reduce and anonymous functions, and how maps
;; work, this is actually not too hard to look at.

;; In fact this is the version of the function that I originally wrote.

;; But I can see it might be a bit off-putting if you thought reduce itself was
;; scary. 

;; Actually the obfuscation / pleasing terseness is all in the anonymous
;; function, and the behaviour of maps, and the reduce bit isn't scary at all.

;; Here's another example, using a little structure as an accumulator.  See if
;; you can figure out what it does using the above ideas to unpack it.

(reduce (fn[[c s] n] [(+ c n), (str s n)]) [0,""] lst)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Clojure's natural facility with abstractions and small functions allows
;; some truly terse code.

;; This little piece of code counts words in a file and orders them by popularity:

(sort #(< (%1 1) (%2 1)) 
      (reduce #(assoc %1 %2 (inc (%1 %2 0))) {}
       (clojure.contrib.string/split 
        #"\W" 
        (slurp "/home/john/hobby-code/reduce.clj"))))

;; With practice this sort of thing is actually readable. Promise!

;; But if I was actually writing it for someone else to read, 
;; I'd probably split it up and give the bits names.

(let [filecontents (slurp "/home/john/hobby-code/reduce.clj")
      words        (clojure.contrib.string/split #"\W" filecontents)
      wordmap      (reduce #(assoc %1 %2 (inc (%1 %2 0))) {}  words)
      sortedwords  (sort #(< (%1 1) (%2 1)) wordmap)]
  sortedwords)

;; And if I knew the library, I'd remember that two of those little operations
;; actually already have names:

(let [filecontents (slurp "/home/john/hobby-code/reduce.clj")
      words        (clojure.contrib.string/split #"\W" filecontents)
      wordmap      (frequencies words)
      sortedwords  (sort-by second wordmap)]
  sortedwords)

;; And then I'd abstract away the function bit

(defn sorted-word-frequencies [string]
  (reverse (sort-by second (frequencies
                   (clojure.contrib.string/split #"\W" string)))))

(take 10 (sorted-word-frequencies (slurp "/home/john/hobby-code/reduce.clj")))


;; Which is also pleasingly terse, but I think more readable.

;; The idiom
( reduce #(assoc %1 %2 (inc (%1 %2 0)) {} .... )

;; which I used to find myself writing all the time, is such a useful thing 
;; that it too has made it into clojure.core as the function frequencies:

(frequencies strlist)

;; But the library version is not quite the same as mine.

;; It's similar, but it uses the new 1.2 features of persistent and transient
;; collections to speed up the operation:

;;Here's the source code:
(defn frequencies
  "Returns a map from distinct items in coll to the number of times
  they appear."
  {:added "1.2"}
  [coll]
  (persistent!
   (reduce (fn [counts x]
             (assoc! counts x (inc (get counts x 0))))
           (transient {}) coll)))

;; This persistent/transient version is quite a lot faster on short lists,
;; although it doesn't seem to have much of an advantage on long ones.




  



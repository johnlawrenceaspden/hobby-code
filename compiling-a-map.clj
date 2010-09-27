;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheating on a Map Lookup. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Suppose we have a function:

(defn step-function [x]
  (cond (< x 1) 0
        (< x 2) 1
        (< x 3) 3
        (< x 4) 4
        (< x 6) 3
        (< x 8) 2
        (< x 9) 3
        (< x 10) 3
        (< x 11) 2
        (< x 12) 1
        :else 0))

(step-function 6) ;2

;; Now imagine that the function is to be generated from some data. We might use a
;; map to hold the values at which the value of the function changes:

(def lookup-table {1 1, 2 3, 3 4, 4 3, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0})

;; And given such a map, we might implement the function by this simple program:
;; Take all the keys that are less than or equal to n.
;; If there are none, then give the default value.
;; Otherwise provide the value of the biggest key.
(defn lookup-fn [map default]
  (let [sorted (sort (seq lookup-table))]
    (fn [n]
      (if-let [[k v]
               (last (filter (fn[[k v]] (<= k n)) sorted ))]
        v
        default))))

((lookup-fn lookup-table 0) 6) ;2

;; Here we look up the values  1, 7, 6, and 20 and -10 in our table:

(map (lookup-fn lookup-table 0) '(1 7 6 20 -10))  ;(1 2 2 0 0)

;; 1 is actually in the map, so that goes to 1, 7 is between 6 and 8, so that
;; goes to 6's value of 2, 20 is higher than all the entries, so it gets 12's
;; value of 0. -10 < 1, is lower than all the map entries so we'll give it the
;; default value 0.

;; A quick test:

(defn fn-to-map [fn range]
  "Evaluate fn everywhere in range. Return map of all values to all results."
  (apply sorted-map
         (apply concat 
                (partition 2 (interleave range (map fn range))))))

(fn-to-map (lookup-fn lookup-table 0) (range -1 14))
;;{-1 0, 0 0, 1 1, 2 3, 3 4, 4 3, 5 3, 6 2, 7 2, 8 3, 9 3, 10 2, 11 1, 12 0, 13 0}

(fn-to-map step-function (range -1 14))
;;{-1 0, 0 0, 1 1, 2 3, 3 4, 4 3, 5 3, 6 2, 7 2, 8 3, 9 3, 10 2, 11 1, 12 0, 13 0}

;; At least as far as integers go, the map and the function are interchangeable.

;; But how fast is it?

;; This is the hard coded version:
(time (doall (map step-function (range 1000))))
"Elapsed time: 3.054365 msecs"

;; And here's our version:
(time (doall (map (lookup-fn lookup-table 0) (range 1000))))
"Elapsed time: 15.657764 msecs"

;; Not bad, but what if we wanted a performant version, to use on trillions of
;; data points?
;; Either version would take hours to run.

;; And what if we were expecting our map to grow and grow until running
;; down the map list one by one was an insane option? This would affect the hard
;; coded version too, of course. The difference is only a constant factor.

;; Well, a binary search is one way to deal with the growth in the map.
;; We could visualize it like this:
;; (1 2 3 4 6 8 9 10 11 12)
;; test >= 8
;; Dividing and conquering at each step:
;; ((1 2 3 4 6) (8 9 10 11 12))
;; test >= 3 or 10
;; Looking in the two halves
;; (((1 2) (3 4 6)) ((8 9) (10 11 12)))
;; test >= 1, 4, 8, or 11
;; Making finer and finer distinctions at each step
;; ((((1) (2)) ((3) (4 6))) (((8) (9)) ((10) (11 12))))
;; test >= 6 or 12
;; ((((1) (2)) ((3) ((4) (6)))) (((8) (9)) ((10) ((11) (12)))))
;; and we're done in at most four steps.
;;
;; The corresponding values are:
;; ((((1) (2)) ((3) ((4) (6)))) (((8) (9)) ((10) ((11) (12)))))
;; ((((1) (3)) ((4) ((3) (2)))) (((3) (3)) ((2 ) ((1 ) (0 )))))
;; so the sane way of proceeding would probably be to write a binary search
;; function and call it a day.


;; But if we really really needed it to be fast, why not:
(defn lookup-fn-handwritten [x]
  (if (< x 6) 
    (if (< x 3)                         ; x is < 6
      (if (< x 2)                       ; x is < 3
        (if ( < x 1)                    ; x is < 2
          0                             ; < 1
          1)                            ; 1 <= x < 2
        3)                              ; 2 <= x < 3
      (if (< x 4)                       ; 3 <= x < 6
        4                               ; 3 <= x < 4
        2))                             ; 4 <= x < 6
    (if (< x 10)                        ; 6 <= x < 10
      (if (< x 9)                       ; 6 <= x < 9
        (if (< x 8) 
          2                             ; 6 <= x < 8
          3)                            ; 8 <= x < 9
        3)                              ; 9 <= x < 10
      (if (< x 11)                      ; 10 < x
        (if (< x 12)                    ; 11 <= x
          1                             ; 11 <= x < 12
          0)
        0))))                           ; 12 <= x
          
;; I have seen this sort of code occasionally in dark corners.  When a man knows
;; how his processor works, knows how his C compiler works, knows about data
;; structures, and really, really needs his loops to be fast then he will
;; occasionally write this sort of thing.

;; This is sort of code that Real Programmers write.

;; A quick test:
(fn-to-map lookup-fn-handwritten (range -1 14))
{-1 0, 0 0, 1 1, 2 3, 3 4, 4 2, 5 2, 6 2, 7 2, 8 3, 9 3, 10 1, 11 0, 12 0, 13 0}

;; It works and it's fast. Already it's faster than the original cond, and its
;; performance advantage will only increase as the map grows:
(time (doall (map lookup-fn-handwritten (range 1000))))
"Elapsed time: 1.442812 msecs"

;; I'd hope it would be faster than the binary search, because it's implementing
;; the same algorithm but replacing a lot of indirections with branches in the
;; code.

;; Every reference into the map gets replaced by a simple 'less than' test and
;; possibly a jump.

;; Why not, indeed?

;; Well, first of all because it's wrong:
(=
 (fn-to-map lookup-fn-handwritten (range -1 14))
 (fn-to-map (lookup-fn lookup-table 0) (range -1 14))) ; false

;; Go on, find the bug. I dare you.

;; Such code is horrible to write and impossible to read.  We could do it, if we
;; really needed to, but it would be mechanical, repetitive, boring and error
;; prone.

;; And if we were gathering the data for the lookup table as part of our
;; program, we wouldn't be able to hand code a special function every time.

;; Hmmmmmmm...

;; Let's look at some easy cases of an imaginary program to write the code for
;; us:


;; The easiest case is:
'(make-lookup-fn {} default)
;->
'default

;; The second easiest is:
'(make-lookup-fn {10 yo} default)
;->
'(fn[x] (if (< x 10) default yo))

;; The third easiest is
'(make-lookup-fn  {8 hey 10 yo 12 hi} default)
;; Which we could represent as:
'(fn[x] (if (< x 10)
          (make-lookup-fn {8 hey} default)
          (make-lookup-fn {12 hi} yo)))

;; oh hell, let's just write it:

(defn make-lookup-expression [var lookup-map lowdefault]
  (let [vmap (sort (seq lookup-map))
        vmcount (count vmap)]
    (cond ;; base cases
     (= vmcount 0) lowdefault
     (= vmcount 1) (let [[test high] (first vmap)]
                          (list 'if (list '< var test) lowdefault high))
     :else ;; recursion (divide map at a pivot element half way along)
     (let [pivot (int (/ (count vmap) 2))
           [test highdefault] (nth vmap pivot)
           before-pivot (take pivot vmap)
           after-pivot  (drop (inc pivot) vmap)]
       (list 'if (list '< var test) ;; and generate an if that chooses which half
             (make-lookup-expression var before-pivot lowdefault)
             (make-lookup-expression var after-pivot  highdefault))))))

;; I actually found that easier to write than the hand-written nest of if
;; statements above.  It all just seemed to fit together according to plan.

;; Let's try it on our example lookup table:
(make-lookup-expression 'x (sort (seq {1 1, 2 3, 3 4, 4 3, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0})) 'default)

(if
 (< x 8)
 (if
  (< x 3)
  (if (< x 2) (if (< x 1) default 1) 3)
  (if (< x 6) (if (< x 4) 4 3) 2))
 (if (< x 11) (if (< x 10) (if (< x 9) 3 3) 2) (if (< x 12) 1 0)))

;; Looks like the sort of thing.


;; If we can generate the code for the nest of ifs, we can generate the code for
;; a lookup function: We shouldn't use x as the variable though, just in case it
;; somehow finds its way into the map! Let's use a gensym for the variable so
;; that it can't capture anything:

(defn make-lookup-fn [lookup-map default]
  (let [var (gensym)]
    (list 'fn [var] (make-lookup-expression var lookup-map default))))

;; The compiler is with us always, so we can turn that into a real function:
(def lookup-fn-automatic (eval (make-lookup-fn {1 1, 2 3, 3 4, 4 3, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0} 0)))

;; The bug is banished:
(=
 (fn-to-map lookup-fn-automatic (range -1 14))
 (fn-to-map (lookup-fn lookup-table 0) (range -1 14))) ;true!

;; Compilers don't make those sorts of mistakes.

;; So we now have the advantages of hard-coding, without the drawbacks. If we
;; can construct our map, then we can construct our hand-coded function, it's
;; just that it's being hand-coded by the compiler at runtime, which is the best
;; sort of hand coding.

;; And it seems to do the business, speed-wise
(def million (doall (range 1000000)))

(time (dorun (map lookup-fn-automatic million)))
"Elapsed time: 778.459478 msecs"

;; Just for comparison:
(time (dorun (map #(* 3 %) million)))
"Elapsed time: 474.40039 msecs"

;; So it seems that our lookup is now comparable in speed to multiplication.
;; In terms of cycles, 778 milliseconds for 1000000 operations means
;; 778 nanoseconds per operation, which is about (* 4.33 778)
;; 3300 cpu cycles per operation with my processor running at 4.33 GHz

;; But we're still doing generic arithmetic on arrays of boxed objects.
;; There is a two orders of magnitude cost for that sort of thing, which
;; is why dynamic languages are often thought to be slow.

;; Let's have a look at how we speed things up on the occasions when we need to.
;; We end up writing code that looks like optimized C, but in return we get
;; optimized C speeds.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optimizing Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Things go faster if we work on primitive integers and native arrays, although
;; the semantics of this are surprisingly subtle. The compiler will not compile
;; literals like 0 to primitive constants without being told to. I don't
;; understand why.

;; It's better to bind them to local variables, at which point the extremely
;; clever HotSpot JVM will notice and optimize at runtime!

;; ACTUALLY I DONT KNOW IF YOU NEED TO. IT HELPS A BIT. JUST (int 0) HELPS.

;; Also, primitives will not survive function calls, which means that we have to throw
;; away the advantages of first order functions and abstractions like map,
;; and stick everything in a big loop, as if we were writing Java or C.

;; I'm told that these little idiosyncracies are being worked on....

;; Here's an example loop, using the primitive types, but without the
;; type hints that reassure the compiler that there's nothing fishy going on.
(last (let [source (into-array Integer/TYPE (range 1000))
            destination (aclone source)]
        ;; after the arrays are created, time the inner loop
        (time (loop [x 0]
                (if (< x (alength source))
                  (do (aset destination x (* 3 (aget source x)))
                      (recur (inc x))))))
        destination))
"Elapsed time: 168.434663 msecs"
;; three quarters of a million machine cycles per loop!

;; Take all the constants out of the inner loop, type hint them as integers And
;; use unchecked-inc for the loop variable. It's an array. I don't think they
;; can be larger than integer size.  Integer/MAX_VALUE is about 2 billion so it
;; wouldn't fit in my 1GB memory anyway.
(last (let [source (int-array (range 1000))
            destination (int-array (aclone source))
            length (alength source)
            zero (int 0) three (int 3)]
        ;; after the arrays are created, time the inner loop
        (time (loop [x zero]
                (if (< x length)
                  (do (aset destination x (* three (aget source x)))
                      (recur (unchecked-inc x))))))
        destination))
"Elapsed time: 1.1944 msecs"
2997
;; 5000 cycles per loop. Still not brilliant. It's great fun removing these optimizations
;; one by one until suddenly the whole thing becomes 100 times slower!

;; Now we've speeded it up, we can use it on much larger arrays. Try length 1000000 now.
(last (let [source (int-array (range 1000000))
            destination (aclone source)
            length (alength source)
            zero (int 0) three (int 3)]
        ;; after the arrays are created, time the inner loop
        (time (loop [x zero]
                (if (< x length)
                  (do (aset destination x (* three (aget source x)))
                      (recur (unchecked-inc x))))))
        destination))
"Elapsed time: 45.465962 msecs"
2999997
;; 200 cycles per loop.

;; The weird thing is that the loop seems to be sub-linear at first

;; And actually we're only down to 200 cycles/multiply even now. I guess we're
;; reading and writing from RAM all the time?

;; However:

(time (int-array 1000000))
"Elapsed time: 5.84744 msecs"

;; Since the looping, multiplying and mapping is now only taking ten times longer than it
;; takes to allocate a suitable destination array in the first place, let's
;; call that a wrap, even though there might be another factor of ten hiding in there somewhere!

;; I'm told that this should be as fast as the equivalent Java.
;; I wonder if that's true? Only one way to find out, I suppose....

;; Note to self: Write Java version and benchmark it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheating Optimally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; So what would the handwritten map lookup loop look like in clojure's
;; equivalent of assembly language?  The nasty bit is that we have to hard-code
;; the constants, and we're going to need a lengthy let-expression and an
;; if-expression transformed to use the local variables.

;; First, let's generate the list of all the constants in the map, as well as
;; the default value From that we can generate the binding form for the big
;; let-expression, and a map of which constants have been bound to which local
;; variables.
(defn constant-helper [mp default]
  (let [constants (sort (set (cons default (apply concat (sort (seq mp))))))
        constants-let (apply vector (mapcat #(list (symbol (str "n" %))(list 'int  %)) constants))
        constant-symbols (map #(symbol (str "n" %)) constants)
        constants-symbols-map (apply sorted-map (interleave constants constant-symbols))]
    (list constants-let constants-symbols-map)))

;; Trying this on our example map
(constant-helper {1 1, 2 3, 3 4, 4 3, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0} 255)
;; We can see what it does:
([n0 (int 0) n1 (int 1) n2 (int 2) ............. n12 (int 12) n255 (int 255)]
   {0 n0, 1 n1, 2 n2, 3 n3, 4 n4, 6 n6, 8 n8, 9 n9, 10 n10, 11 n11, 12 n12, 255 n255})


;; To put the constants into the expression, once it's generated, we can use the
;; code-walker function from clojure walk:

;; Here's a simple example of a code-walk, or tree map as it's sometimes known.
(clojure.walk/postwalk
 #(if (integer? %) (get {1 "1", 2 "2"} % %) %)
 '(+ 1 (* 2 3)))

;; Gives:
;; (+ "1" (* "2" 3))
;; See how it's changed 1 and 2 but left everything else alone?

;; So we can make both the let-expression and the transformed nest of ifs with
(defn transformed-exprs [mp default]
  (let [[let-expr cs-map] (constant-helper mp default)
        if-expr (make-lookup-expression 'x (sort (seq mp)) default)
        transformed-if-expr (clojure.walk/postwalk
                             #(if (integer? %) (get cs-map % %) %)
                             if-expr)]
    (list transformed-if-expr let-expr)))


;; Evaluating:
(transformed-exprs {1 1, 2 3, 3 4, 4 3, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0} 255)
;; Gives us:
((if (< x n8)   ...   (if (< x n9) n3 n3) n2) (if (< x n12) n1 n0)))
 [n0 (int 0) n1 (int 1) ... n12 (int 12) n255 (int 255)])
;; Which are the parts we need to make an optimal loop:

;; so the final expression we're looking at would be:

(let [source (int-array million)
      destination (aclone source)
      length (alength source)]
        (let  [n0 (int 0) n1 (int 1) n2 (int 2) n3 (int 3) n4 (int 4) n6 (int 6) n8 (int 8) n9 (int 9) n10 (int 10) n11 (int 11) n12 (int 12) n255 (int 255)]
          (time 
           (loop [i (int 0)]
             (if (< i length)
               (do (aset destination i (let [x (aget source i)]
                                         (if (< x n8) (if (< x n3) (if (< x n2) (if (< x n1) n255 n1) n3) (if (< x n6) (if (< x n4) n4 n3) n2)) (if (< x n11) (if (< x n10) (if (< x n9) n3 n3) n2) (if (< x n12) n1 n0)))))
                   (recur (unchecked-inc i)))))))
        destination)

"Elapsed time: 28.387293 msecs"
nil

;; Which somewhat to my amazement is not only executable and produces the correct answer,
;; but actually seems faster than the multiplication example! (something like 100 cycles/loop).

;; So the final step is to take a map and a default value, and generate the
;; expression above, which takes a java int array as input and gives back
;; another one, with all the values passed through the lookup table.


(defn generate-array-transformer [mp default]
  (let [[if-expr let-expr] (transformed-exprs mp default)]
       `(fn[source#]
          (let [source# (int-array source#)
                destination# (aclone source#)
                length# (alength source#)]
            (time (let  ~let-expr
              (loop [i# (int 0)]
                (if (< i# length#)
                  (do (aset destination# i# (let [~'x (aget source# i#)] ~if-expr))
                   (recur (unchecked-inc i#)))))))
        destination#))))

;; Here's how we use it to make the loop code
(generate-array-transformer {1 1, 2 3, 3 4, 4 3, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0} 255)

;; And now we'll compile it and assign it to a suitably named variable
(def never-going-to-work (eval (generate-array-transformer {1 1, 2 3, 3 4, 4 3, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0} 255)))

;; Let's create a java array with our data in it
(def million-ints (int-array million))

(take 100 (never-going-to-work million-ints))
"Elapsed time: 11.656722 msecs"
;;(255 1 3 4 3 3 2 2 3 3 2 1 0 0 0 ......
;; bloody hell!

;; Not only does it appear to be working, but the inner loop now appears to be
;; down to twelve milliseconds.  50 cycles per number transformed.

;; However, if we time the whole thing:
(time (never-going-to-work million-ints))
"Elapsed time: 10.887267 msecs"
"Elapsed time: 148.408179 msecs"

         
;; I'm very happy with that, considering that I've managed to optimize away a
;; lookup into a data structure.

;; But it's annoying that the whole loop actually takes 148ms.

;; Most of the time is being spent in the call to int-array. But the call to
;; int-array is only there so that the compiler can tell it's an int array!

;; The thing passed in is an int-array already! It doesn't need to spend this
;; time transforming it!

;; How do I let the compiler know that it's actually going to get an int-array passed in?

;; Just to prove that it wasn't a fluke, here's a different map
         
(def never-going-to-work-2 (eval (generate-array-transformer {1 99, 2 33, 3 4, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0, 15 -1, 24 100} 100)))

(time (take 100 (never-going-to-work-2 million-ints))) ;(100 99 33 4 4 4 2 2 3 3 2 1 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 100 100 ......
;;inner loop:
"Elapsed time: 11.455121 msecs"
;;whole call:
"Elapsed time: 138.169579 msecs"

;; Just to check that it's actually doing something, and that we're not being fooled by
;; some sort of lazy eval
(def ten-million-ints (int-array (apply concat (repeat 400000 (range 25) ))))

(time (take 100 (never-going-to-work-2 ten-million-ints)))
"Elapsed time: 156.814438 msecs"
"Elapsed time: 1437.094696 msecs"
;; (100 99 33 4 4 4 2 2 3 3 2 1 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 100 100 99 33 4 4 ...

;; As a stiffer test, let's make a completely random map with 100 entries:
(def random-map (apply sorted-map (for [i (range 200)] (rand-int 100))))

;; And generate and compile code to inline the binary search
(def large-random-step-function (eval (generate-array-transformer random-map 100)))

;; Let's see how it does:
(time (take 100 (large-random-step-function ten-million-ints)))
"Elapsed time: 276.477877 msecs"
"Elapsed time: 1497.910398 msecs"
;;(19 19 12 29 28 28 28 4 72 99 99 87 87 ............

;; 119 cycles per lookup over 10 million integers. The whole loop in less than a second,
;; although there's still this completely silly second and a half where it's turning an array
;; of ints into an identical array of ints.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wild Speculation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; And so I wonder:

;; This technique strikes me as very general, and very useful. All sorts of things can be
;; represented as lookups in tables.

;; This whole program took me one short day to write, and the whole time I was doing things
;; that I've never done before, just going by intuition.

;; I think that the program should be as fast as the equivalent java program would be, although
;; I haven't got around to actually testing that, so I may have dropped the ball somewhere.

;; In any case, it's probably possible to generate code like this that does run
;; as fast as whatever the fastest Java implementation actually is.

;; The JVM is widely thought to be around the same speed as native machine code
;; or optimized C.

;; I'm absolutely sure that I'm not able to write the equivalent program in
;; Java, C, or assembler without code-generation.

;; The code generation would be very very very much harder in Java, C or
;; assembler.

;; And so I wonder, is Clojure the fastest computer language in the world?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bugger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you try to make the map much larger than 100 entries
;; e.g.
(def random-map (apply sorted-map (for [i (range 400)] (rand-int 1000))))

;; Then when you try to compile it:
(def large-random-step-function (eval (generate-array-transformer random-map 100)))
;; The compilation fails with this interesting error:
Too many arguments in method signature in class file user$eval23503$fn__23504$fn__23505
[Thrown class java.lang.ClassFormatError]

;; Something tells me that the let-bound local variables are getting translated
;; to a function call (let/lambda equivalence and all that), and that Java has a
;; hard coded limit somewhere. May I guess that that limit is 256?

;; I don't know whether there's any way round that. I'm too tired to think.









 



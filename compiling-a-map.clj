;;Suppose we have a lookup table, in this case a map

(def lookup-table {1 1, 2 3, 3 4, 4 3, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0})

;; We intend to use it as a step function , so:

;; -10 < 1, is lower than all the map entries so we'll give it the default
;; value.  1 is in the map, so that goes to 1, 7 is between 6 and 8, so that
;; goes to 6's value of 2, 20 is higher than all the entries, so it gets 12's value of 0.


(defn lookup-fn [map default]
  (fn [n]
    (if-let [r (last (filter (fn[[k v]] (<= k n)) (sort (seq lookup-table))))] (second r)
          default)))


;; A quick test

(defn fn-to-map [fn range]
  (apply sorted-map
         (apply concat 
                (partition 2 (interleave range (map fn range))))))

(fn-to-map (lookup-fn lookup-table 0) (range -1 14))

{-1 0, 0 0, 1 1, 2 3, 3 4, 4 3, 5 3, 6 2, 7 2, 8 3, 9 3, 10 2, 11 1, 12 0, 13 0}


(time (doall (map (lookup-fn lookup-table 0) (range 1000))))
"Elapsed time: 27.675279 msecs"

;; Not bad, but what if we wanted a performant version, to use on a trillion
;; data points?  And what if we were expecting our map to grow until running
;; down the map list one by one was an insane option?

;; Well, a binary search is one way,
(1 2 3 4 6 8 9 10 11 12)
test >= 8
((1 2 3 4 6) (8 9 10 11 12))
test >= 3 or 10
(((1 2) (3 4 6)) ((8 9) (10 11 12)))
test >= 1, 4, 8, or 11
((((1) (2)) ((3) (4 6))) (((8) (9)) ((10) (11 12))))
test >= 6 or 12
((((1) (2)) ((3) ((4) (6)))) (((8) (9)) ((10) ((11) (12)))))
;; and we're done in three steps, with corresponding values
((((1) (2)) ((3) ((4) (6)))) (((8) (9)) ((10) ((11) (12)))))
((((1) (3)) ((4) ((3) (2)))) (((3) (3)) ((2 ) ((1 ) (0 )))))
;; so the sane way of proceeding would probably be to write a binary search
;; function and call it a day.


;; But if we really really needed it to be fast, why not:

(defn lookup-fn-handwritten [x]
  (if (< x 6) 
    (if (< x 3); x is < 6
      (if (< x 2) ; x is < 3
          (if ( < x 1) ; x is < 2
            0 ; < 1
            1)       ; 1 <= x < 2
          3) ; 2 <= x < 3
      (if (< x 4) ; 3 <= x < 6
        4   ; 3 <= x < 4
        2)) ; 4 <= x < 6
    (if (< x 10) ; 6 <= x < 10
      (if (< x 9) ; 6 <= x < 9
        (if (< x 8) 
          2   ; 6 <= x < 8
          3)  ; 8 <= x < 9
        3)    ; 9 <= x < 10
      (if (< x 11)  ; 10 < x
        (if (< x 12) ; 11 <= x
          1 ; 11 <= x < 12
          0)
        0)))) ; 12 <= x
          
;; I have seen this sort of code occasionally in dark corners.  When a man knows
;; how his processor works, knows how his C compiler works, knows about data
;; structures, and really, really needs his loops to be fast then he will
;; occasionally write this sort of thing. This is sort of code that *real*
;; programmers write. 

;; A quick test:
(fn-to-map lookup-fn-handwritten (range -1 14))
{-1 0, 0 0, 1 1, 2 3, 3 4, 4 2, 5 2, 6 2, 7 2, 8 3, 9 3, 10 1, 11 0, 12 0, 13 0}

(time (doall (map lookup-fn-handwritten (range 1000))))
"Elapsed time: 1.981008 msecs"

;; Why not, indeed?

;; Well, first of all because it's wrong:
(=
 (fn-to-map lookup-fn-handwritten (range -1 14))
 (fn-to-map (lookup-fn lookup-table 0) (range -1 14))) ; false

;; Go on, find the damned error. I dare you.

;; Such code is horrible to write and impossible to read. We could do it, if we really needed to,
;; but it would be mechanical, repetitive, boring and error prone.

;; Hmmmmmmm...


;; Let's look at some easy cases

(make-lookup-fn [] default)
;->
default

(make-lookup-fn {10 yo} default)
;->
(fn[x] (if (< x 10) default yo))
;; or we could write it:
(fn[x] (if (< x 10) (make-lookup-fn {} default) yo))
;;
(make-lookup-fn  {8 hey 10 yo 12 hi} default)
;;->
(fn[x] (if (< x 10)
         (make-lookup-fn {8 hey} default)
         (make-lookup-fn {12 hi} yo)))

;; oh hell, let's just write it.

(defn make-lookup-expression [var vmap lowdefault]
  (let [vmcount (count vmap)]
    (cond (= vmcount 0) lowdefault
          (= vmcount 1) (let [[test high] (first vmap)]
                          (list 'if (list '< var test) lowdefault high))
          :else
          (let [pivot (int (/ (count vmap) 2))
                pre-pivot (dec pivot)
                pre-pivot-element (nth vmap pre-pivot)
                [test highdefault] (nth vmap pivot)
                before-pivot (take pivot vmap)
                after-pivot  (drop (inc pivot) vmap)]
            (list 'if (list '< var test)
                  (make-lookup-expression var before-pivot lowdefault)
                  (make-lookup-expression var after-pivot highdefault))))))

;; I actually found that easier to write than the hand-written loop above. It all just seemed to fit together.
;; Let's try it on our example
(make-lookup-expression 'x (sort (seq {1 1, 2 3, 3 4, 4 3, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0})) 'default)

(if
 (< x 8)
 (if
  (< x 3)
  (if (< x 2) (if (< x 1) default 1) 3)
  (if (< x 6) (if (< x 4) 4 3) 2))
 (if (< x 11) (if (< x 10) (if (< x 9) 3 3) 2) (if (< x 12) 1 0)))

;; Looks like the sort of thing. We shouldn't use x as a variable though, just in case it somehow finds its way into the map!

(defn make-lookup-fn [map default]
  (let [vmap (sort (seq map))
        var  (gensym)]
    (list 'fn [var] (make-lookup-expression var vmap default))))

;; Luckily, the compiler is with us always:

(def lookup-fn-automatic (eval (make-lookup-fn {1 1, 2 3, 3 4, 4 3, 6 2, 8 3, 9 3, 10 2, 11 1, 12 0} 0)))

;; Bug banished:

(=
 (fn-to-map lookup-fn-automatic (range -1 14))
 (fn-to-map (lookup-fn lookup-table 0) (range -1 14))) ;true!

;; And it seems to do the business
(def million (doall (range 1000000)))

(time (doall (map lookup-fn-automatic million)))
"Elapsed time: 790.041511 msecs"

;; Just for comparison:
(time (doall (map #(* 3 %) million)))
"Elapsed time: 603.250889 msecs"

;; Now, our lookup works at about the same speed as arithmetic, which is to say
;; About 603 nanoseconds per operation, which is about 2500 cpu cycles per multiply.

;; But we're still doing generic arithmetic, 

;; Things work faster if we work on primitive integers
(def million (int-array (range 1000000)))







(time (doall (amap million x newmillion (* 3 x))))
"Elapsed time: 605.47959 msecs"

(time (doall (map (fn[x] (let [x (int x)] (* 3 x))) million)))
"Elapsed time: 748.628281 msecs"

(time (doall (map (fn[x] (let [x (int x) t (int 3)] (* t x))) million)))





                
                
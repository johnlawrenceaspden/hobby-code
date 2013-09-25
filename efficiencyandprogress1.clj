;; Efficiency and Progress
;; Are ours once again
;; Now that we have the neut-ron bomb
;; It's nice and quick and clean and ge-ets things done...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When you program in Clojure, you get the raw speed of assembler.

;; Unfortunately, that is, assembler on a ZX81, running a Z80 processor at 4MHz in 1981.

;; If anything, that comparison is unfair to my old ZX81. Does anyone
;; remember '3D Invaders', a fast and exciting first person shooter /
;; flight simulator that ran in 1K of RAM *including memory for the
;; screen*?

;; Once upon a time, I had the knack of making clojure run at the same
;; speed as Java, which is not far off the same speed as C, which is
;; not far off the speed of the sort of hand-crafted machine code which
;; no-one in their right mind ever writes, in these degenerate latter
;; days which we must reluctantly learn to call the future.

;; But I seem to have lost the knack. Can anyone show me what I am doing wrong?

;; At any rate, it isn't too hard to get it to run at something like
;; the real speed of the machine, as long as you're prepared to write
;; code that is more like Java or C than Clojure.

;; So here are some thoughts about how to do this.

;; Which I offer up only as a basis for discussion, and not in any way
;; meaning to stir up controversy, or as flame-bait or ammunition for
;; trolls or anything of that sort.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clojure is very slow:

(time (reduce + (map + (range 1000000) (range 1000000))))
"Elapsed time: 5316.638869 msecs"
;-> 999999000000

;; The greater part of its slowness seems to be do with lazy sequences

(time (def seqa (doall (range 1000000))))
"Elapsed time: 3119.468963 msecs"
(time (def seqb (doall (range 1000000))))
"Elapsed time: 2839.593429 msecs"

(time (reduce + (map + seqa seqb)))
"Elapsed time: 3558.975552 msecs"
;-> 999999000000

;; It looks as though making a new sequence is the expensive bit
(time (doall (map + seqa seqb)))
"Elapsed time: 3612.553803 msecs"
;-> (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 ...)

;; Just adding things up is way faster
(time (reduce + seqa))
"Elapsed time: 315.717033 msecs"
499999500000


;; I wondered if there was a way of avoiding lazy-seqs

(time (def veca (vec seqa)))
"Elapsed time: 470.512696 msecs"

(time (def vecb (vec seqb)))
"Elapsed time: 374.796054 msecs"

;; After all, 'use the right data structure for the problem' is pretty much lesson 1, and if vectors are not a good data structure
;; for this problem, then what is?

;; But it seems that despite the speed of making the vectors, it doesn't help much when we do our thing.
;; In fact it's a bit slower
(time (reduce + (mapv + veca vecb)))
"Elapsed time: 4329.070268 msecs"
999999000000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So lets say 3600ms to add together two arrays of 1000000 elements and sum the result.

;; In C on the same machine (my little netbook with its 1.66GHz Atom
;; and 512kb cache) this seems to take 16 ms, being 8ms for the map
;; and 8 ms for the reduce.  I'm assuming that that time is mostly
;; spent waiting on the main memory, but I may be wrong. Who knows how
;; these things are done?

(/ 3600 16) ;-> 225

;; So shall we call this a 225x slowdown for the natural expression in
;; the two languages of mapping and reducing?

(time (reduce + seqa))
"Elapsed time: 358.152249 msecs"
499999500000

;; If we just look at the reduction, then that's 
(/ 358 8.) ; 44.75


;; So around 50x


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


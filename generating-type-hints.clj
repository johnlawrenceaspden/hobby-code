;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meikel's answer to my question: How to get rid of the call to int-array?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In my last post, I posed a question about how to get rid of a call to
;; int-array in some generated code.

;; I was using it to let the compiler know that a certain array was a Java array
;; of ints.

;; Normally, you'd do this with the type hint ^ints, but I was generating the
;; code, and couldn't make it work.

;; Meikel Brandmeyer ( http://kotka.de/blog/ ) not only answered my question,
;; but through sheer patience managed to get the answer through my skull, and so
;; I'm writing it down here as a reference for the next time I can't remember.

;; Using type hints turns out to be tricky from generated code, because
;; type hints are metadata. However once you've seen the trick it's OK.

;; Here's a model problem:

;; Some arrays of java ints:
(def thousand-ints    (int-array 1000     (range)))
(def million-ints     (int-array 1000000  (range)))
(def ten-million-ints (int-array 10000000 (range)))


;; A function that looks at them, but does nothing.
(defn do-nowt [a]
  (let [len (alength a)]
     (loop [i 0]
       (when (< i len)
         (aget a i)
         (recur (inc i))))))

;; As if the array were the Medusa, just looking turns the function to stone:
(time (do-nowt thousand-ints))
"Elapsed time: 46.054186 msecs"
; 200000 cycles/loop!!! What is it doing??

;; How to make it fast?
(defn do-nowt-quickly [^ints a]
  (let [len (alength a)]
     (loop [i (int 0)]
       (when (< i len)
         (aget a i)
         (recur (inc i))))))

(time (do-nowt-quickly thousand-ints))
"Elapsed time: 0.109981 msecs"
; 433 cycles/loop

;; How fast can HotSpot make it over a long run?
(time (do-nowt-quickly ten-million-ints))
"Elapsed time: 266.500137 msecs"
;; 115 cycles/loop

;; If the array's all zeroes, it can actually get it down to 27. What is that
;; about? Is it optimizing away *looking at the array*?  If not, what is so
;; special about zeros that you can look at them faster?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We need to apply the function int to the initializer for the loop variable
;; and the type-hint ^ints to the input variable. 

;; How to generate the code rather than writing it by hand?

;; One's first thought is just to take the fast function, and use it as a
;; template for syntax-quote and auto-gensyms:

(defn mek-thing-as-does-nowt-quickly[]
  `(fn[^ints a#]
     (let [len# (alength a#)]
       (loop [i# (int 0)]
         (when (< i# len#)
           (aget a# i#)
           (recur (inc i#)))))))


;; However:
#_ (eval (mek-thing-as-does-nowt-quickly))

;; Unable to resolve classname: clojure.core/ints
;;   [Thrown class java.lang.IllegalArgumentException]

;; I don't know what is going on here.

;; I fear the reader and the metadata it brings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Mirror of Perseus:

(defn mek-another-thing-as-does-nowt-quickly[]
  `(fn[^"ints" a#]
     (let [len# (alength a#)]
       (loop [i# (int 0)]
         (when (< i# len#)
           (aget a# i#)
           (recur (inc i#)))))))

(let [f (eval (mek-another-thing-as-does-nowt-quickly))]
  (time (f ten-million-ints)))
"Elapsed time: 282.178954 msecs"

;; Meikel pointed out that "[I" is a synonym for "ints". I don't know why he
;; thought that was important, since they both seem to work, but that probably
;; means that there's still something going on here that I haven't understood,
;; so I mention it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Entertaining Variation:

;; Here we use a gensym for the function argument rather than an auto-gensym.
;; This allows for run-time generation of the string "ints" so we can make
;; various specialized functions for arrays of different types.

(defn mek-yet-another-thing-as-does-nowt-quickly[]
  (let [a (gensym)]
    `(fn[ ~(with-meta a {:tag "ints"}) ]
       (let [len# (alength ~a)]
         (loop [i# (int 0)]
           (when (< i# len#)
             (aget ~a i#)
             (recur (inc i#))))))))

(let [f (eval (mek-yet-another-thing-as-does-nowt-quickly))]
  (time (f ten-million-ints)))
"Elapsed time: 271.423182 msecs"






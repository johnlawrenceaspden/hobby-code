;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clojure Macro Tutorial Part II: The Compiler Strikes Back
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WARNING!

;; The first bit of this tutorial was about the idea of using the compiler
;; itself to write code.  This joyful idea has a slightly chequered history, and
;; turned out to be every bit as hard to get right as the idea of a subroutine
;; was.

;; In order to explain exactly how clojure's macro system works, I have felt it
;; necessary to go into the gory details of what goes wrong when you take the
;; naive approach to code generation.

;; As a result, this post is long, difficult, and ends on a depressing note.

;; I've tried (very hard) to make it only as long, and no more difficult than it
;; needs to be.

;; If I were you, I would skip directly to Part III, where clojure gloriously
;; resolves all these problems, and at the same time makes macros very easy to
;; write. Reading that may give you the courage to come back and face the dark
;; code beneath.

;; I am aware that this is really only a first cut at this way of explaining
;; things, and I'd very much welcome suggestions for how I could make it better.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So far, we have been considering the dbg macro:

(defmacro dbg[x] `(let [x# ~x] (println '~x "->" x#) x#))

;; And we have got as far as being able to approximate it by:

(defmacro dbg-1 [s]
  (list 'let ['a s] (list 'println (list 'quote s) "->" 'a) 'a))

;; We have by this point understood the essence of macros, but there are a
;; couple of loose ends to tidy up.

;; We need to learn to use the syntax-quote notation `(let [x# .....) , partly
;; because it is easier to write macros when the notation looks like the code to
;; be produced, and partly because it helps us avoid certain difficulties which
;; have historically been a problem for macro writers.

;; And we need to understand what those difficulties are, so that we can
;; understand what syntax-quote is doing for us and why.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A second problem to solve with macros: C-style for loops
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Before I try to explain syntax-quote, we should look at another simple
;; macro, which is nevertheless complex enough to run into the traditional
;; difficulties of macro-writing.

;; Suppose we find ourselves writing many imperative loops. The sort of thing
;; which C expresses as
;; for(i=0; i<10; i++)
;; {
;;     print "%d" i;
;; }

;;In clojure, we can equivalently write:
(loop [i 0]
  (when (< i 10)
    (print i)
    (recur (inc i))))

;; 012345678910
;; nil

;; Let us see if we can construct a simple macro to take the labour out of
;; reading and writing these little loops using the primitive macro construction
;; methods that we already know.

;; (In effect, we are trying to write a simple version of the doseq macro.)

(comment
;; We would like to be able to write
  (forloop [i 10] (print i))

;; and have it turn into:
  (loop [i 0]
    (when (< i 10)
      (print i)
      (recur (inc i)))))

;; Let us first of all define a code-generating function:
(defn forloop-f [[var finish] & code]
  (list 'loop [var 0]
        (concat (list 'when)
                (list (list '< var finish))
                code
                (list (list 'recur (list 'inc var))))))

;; A quick test
(forloop-f '[i 10] '(print i))
;;evaluates to:
(loop [i 0]
  (when (< i 10)
    (print i)
    (recur (inc i))))

;; which is what we want.

;; So let's make it a macro:
(defmacro forloop-bugs-1 [[var finish] & code]
  (list 'loop [var 0]
        (concat (list 'when)
                (list (list '< var finish))
                code
                (list (list 'recur (list 'inc var))))))


;; And try it out:

(forloop-bugs-1 [i 10] (print i))
;;0123456789
;;nil

(forloop-bugs-1 [j 10] (dbg j))
;; j -> 0
;; j -> 1
;; j -> 2
;; j -> 3
;; j -> 4
;; j -> 5
;; j -> 6
;; j -> 7
;; j -> 8
;; j -> 9
;; nil

;; It seems to have worked! Bingo?

;; It has worked. But it's not called forloop-bugs-1 for nothing.  We will have
;; to work a little to make the bugs show, but they are there.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There are some problems that one runs into when constructing macros in this
;; way, and they are problems that all lisps have to find ways of solving if
;; they want macros.

;; I don't think many people realize quite how clever clojure's namespace and
;; backquote system are. They make what were serious problems with nasty
;; solutions in traditional lisps into minor difficulties in clojure.

;; We should understand the problems, in order to understand the answer, and use
;; clojure's macros with full confidence, rather than thinking of them as some
;; sort of magic.

;; Let's have a closer look at our naively-written loop macro:
(defmacro forloop-bugs-1 [[var finish] & code]
  (list 'loop [var 0]
        (concat (list 'when)
                (list (list '< var finish))
                code
                (list (list 'recur (list 'inc var))))))


;; This macro, simple though it is, is sufficiently complex that it runs
;; into all the traditional difficulties of macros:

;; Once we've ploughed through the difficulties, we'll be able to see what
;; syntax-quote is for, and better appreciate what it's doing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Controlling the evaluation of the arguments
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The first problem is not so much a problem, as it is the very thing we are
;; trying to do: control when the arguments to our macro get evaluated.

;; Consider
(forloop-bugs-1 [i (rand 10)]
                (print i))

;; The intent is to go round the loop a random number of times.
;; Here are some sample evaluations:
;; 0123nil
;; 01234nil
;; 0nil

;; So by all appearances, the macro is doing what it should.

;; But let's use our debugging macro to find out how many times (rand 10) is getting called:
(forloop-bugs-1 [i (dbg (rand 10))]
                (print i))

;; (rand 10) -> 0.9753528272119372
;; 0(rand 10) -> 2.407051391491234
;; 1(rand 10) -> 8.511366087571314
;; 2(rand 10) -> 6.795055112530893
;; 3(rand 10) -> 1.6571396363426516
;; nil

;; Was that what you expected to happen?

;; It seems that (rand 10) is getting called each time we go round the loop.

;; Let's ask the compiler what
(forloop-bugs-1 [i (dbg (rand 10))]
                (print i))
;; expands to, which we can do using the function macroexpand-1:
(macroexpand-1 '(forloop-bugs-1 [i (dbg (rand 10))]
                                (print i)))

;; The generated code turns out to be:
(loop [i 0]
  (when (< i (dbg (rand 10)))
    (print i)
    (recur (inc i))))

;; That's what we thought we wanted, but looking at it now, it's pretty obvious
;; that there's a problem.

;;The code that we should probably have written might look something like this:
(let [finish (dbg (rand 10))]
  (loop [i 0]
    (when (< i finish)
      (print i)
      (recur (inc i)))))

;; Here are some test evaluations of this new code:

;; (rand 10) -> 9.333250125032992
;; 0123456789nil

;; (rand 10) -> 4.260732182937476
;; 01234nil

;; (rand 10) -> 1.6344563853179461
;; 01nil

;; Which is probably what we want, and almost certainly what someone using the
;; macro will expect.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The First Law
;;
;; If you take control of when your arguments are evaluated, you have to take
;; control of when your arguments are evaluated.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So let's modify our macro in the obvious way to do what we now realize that we want:

(defmacro forloop-bugs-2 [[var end] & code]
  (list 'let ['finish end]
        (list 'loop [var 0]
              (concat (list 'when)
                      (list (list '< var 'finish))
                      code
                      (list (list 'recur (list 'inc var)))))))


(forloop-bugs-2 [i (dbg (rand 10))]
                (print i))
;;(rand 10) -> 5.427029108032794
;;012345nil

;; It all seems to be working.
;; Let's have a look at the generated code:

(macroexpand-1 '(forloop-bugs-2 [i (dbg (rand 10))]
                                (print i)))

(let [finish (dbg (rand 10))]
  (loop [i 0]
    (when (< i finish)
      (print i)
      (recur (inc i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Name Collision
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The remaining problems are all to do with the way that the names in the
;; expansion collide with the names in the environment, and in any code which is
;; passed in to the macro.

;; You will notice that in the code generated by forloop-bugs-2, there is always
;; a variable called finish

;; Let's see if we can get that fact to cause problems:

;; Imagine that we want the loop to go all the way to ten, but for some reason
;; we don't want the printing to happen after a cutoff point:

;; We might say:
(let [cutoff 4]
  (forloop-bugs-2 [i 10]
                  (when (< i cutoff) (print i))))
;; 0123nil

;; Now let's change the name of the cutoff variable:
(let [finish 4]
  (forloop-bugs-2 [i 10]
                  (when (< i finish) (print i))))
;; 0123456789nil

;; Ooops.

;; If this doesn't violate the principle of least surprise for the user of the
;; macro, I don't know what would!

;; Again, We should look the macro expansion of the clause (forloop-bugs-2 ....)
(macroexpand-1 '(forloop-bugs-2 [i 10]
                                (when (< i finish) (print i))))
;; which is:
(let [finish 10]
  (loop [i 0] (when (< i finish)
                (when (< i finish) (print i))
                (recur (inc i)))))

;; This makes it pretty clear why this is happening, and why it didn't happen
;; when the variable was called cutoff.

;; One solution to this problem is just to choose names for the variables
;; generated by the macro which are unlikely to collide.

(defmacro forloop-bugs-3 [[var end] & code]
  (list 'let ['forloop-bugs-3-finish end]
        (list 'loop [var 0]
              (concat (list 'when)
                      (list (list '< var 'forloop-bugs-3-finish))
                      code
                      (list (list 'recur (list 'inc var)))))))


;; But this is a pretty poor solution:

;; Firstly, we don't want to write code that will only go wrong very occasionally.
;; That's the worst sort of bug to track down when it finally happens.

;; Secondly, million to one chances come up nine times out of ten. There's a
;; compiler involved.  Who knows what crazy names it will decide to come up
;; with, or what will happen when one macro expands into another macro?

;; So clojure gives us a special mechanism for making silly names which are
;; different each time.

(gensym) ;; G__11330 
(gensym) ;; G__11335

;; And we use that. Needless to say, avoid using names of the form G__????? in
;; your own code. The compiler will too.

;; How shall we use gensym?

(defmacro forloop-bugs-4 [[var end] & code]
  (let [finish (gensym)]
    (list 'let [finish end]
          (list 'loop [var 0]
                (concat (list 'when)
                        (list (list '< var finish))
                        code
                        (list (list 'recur (list 'inc var))))))))


;; Now let's look at the code that is generated by forloop-bugs-4
(macroexpand-1 '(forloop-bugs-4 [i 10]
                                (when (< i finish) (print i))))
'(let [G__11353 10]
   (loop [i 0]
     (when (< i G__11353)
       (when (< i finish)
         (print i))
       (recur (inc i)))))

;; Notice that the finish variable in the macro is now called G__11353, whereas
;; the finish variable in the code block has been preserved.

;; That's how you do macros by hand.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Except for one more little detail... 

;; It can happen that your macro is imported into a namespace where one of the
;; functions that was present where it was defined is either not present, or
;; defined differently.

;; So if we really and truly want to bulletproof our macros, which we do if we
;; are going to rely on them, then actually we should write:

(defmacro forloop [[var end] & code]
  (let [finish (clojure.core/gensym)]
    (clojure.core/list 'clojure.core/let [finish end]
          (clojure.core/list 'clojure.core/loop [var 0]
                (clojure.core/concat (clojure.core/list 'when)
                        (clojure.core/list (clojure.core/list '< var finish))
                        code
                        (clojure.core/list (clojure.core/list 'recur (clojure.core/list 'clojure.core/inc var))))))))


(forloop [i 10] (print i))
;;0123456789nil


;; Now obviously this is going to be a gigantic pain to write.

;; It would be repetitive, mechanical and error prone. Exactly the sort of thing
;; we were trying to avoid by using macros in the first place.

;; I'm actually slightly amazed that I got the abortion above to work at all,
;; and I wouldn't be at all surprised if bugs were found in it. Not conceptual
;; bugs. I think the concept is fine. Just stupid little finger-trouble errors.

;; As I said, exactly the sort of thing we use macros to avoid having to do ourselves.

;; And a macro could be written which would automate the tricky bits of macro writing. 

;; But clojure doesn't use an ordinary macro. It considers that macro-writing is
;; so important that there is special syntax built into the reader.

;; This is what forloop should really look like. It's much easier to write like this,
;; and it looks very like the code we're trying to generate.

(defmacro forloop [[i end] & code]
  `(let [finish# ~end]
     (loop [~i 0]
       (when (< ~i finish#)
         ~@code
         (recur (inc ~i))))))











































 















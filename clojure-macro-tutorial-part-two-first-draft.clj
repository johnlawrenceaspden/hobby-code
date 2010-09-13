;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So far, we have been considering the dbg macro:

(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

;; And we have got as far as being able to approximate it by:

(defmacro dbg-1 [s]
  (list 'let ['a s] (list 'println (list 'quote s) "=" 'a) 'a))

;; We have by this point understood the essence of macros, but there are a
;; couple of loose ends to tidy up.

;; We need to learn to use the syntax-quote notation `(let [x# .....) , partly
;; because it is easier to write macros when the notation looks like the code to
;; be produced, and partly because it helps us avoid certain difficulties which
;; have historically been a problem for macro writers.

;; And we need to understand what those difficulties are, so that we can
;; understand what syntax-quote is doing for us and why.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Before I explain why the syntax-quote is doing what it's doing, we should
;; look at another simple macro, which is nevertheless complex enough to run
;; into the traditional difficulties of macro-writing.

;; There are three potential problems. 

;; Once we've ploughed through the difficulties, we'll be able to see what
;; syntax-quote is for, and better appreciate what it's doing.

;; Suppose we find ourselves writing many imperative loops. The sort of thing
;; which C expresses as
;; for(i=0; i<=10; i++)
;; {
;;     print "%d" i;
;; }

;;In clojure, we can equivalently write:
(loop [i 0]
  (when (<= i 10)
    (print i)
    (recur (inc i))))

;; 012345678910
;; nil

;; Let us see if we can construct a simple macro to take the labour out of
;; reading and writing these little loops using the primitive macro construction
;; methods that we already know.

;; In effect, we are trying to write a simple version of the doseq macro.

;; We would like (fori 10 (print i))

;; to turn into:
(loop [i 0]
  (when (<= i 10)
    (print i)
    (recur (inc i))))

;; Let us first of all define a code-generating function:
(defn fori-f [finish & code]
  (list 'loop '[i 0]
        (concat
         (concat (list 'when) (list (list '<= 'i finish))
                 code)
         (list (list 'recur '(inc i))))))

;; A quick test
(fori-f 10 '(print i))
;; evaluates to:
(loop [i 0]
  (when (<= i 10)
    (print i)
    (recur (inc i))))
;; which is what we want.

;; So let's make it a macro:

(defmacro fori-bugs-1 [finish & code]
  (list 'loop '[i 0]
        (concat
         (concat (list 'when) (list (list '<= 'i finish))
                 code)
         (list (list 'recur '(inc i))))))

;; And try it out:

(fori-bugs-1 10 (print i))
;;012345678910
;;nil

;; It seems to have worked! Bingo?

;; It has worked. But it's not called fori-bugs-1 for nothing.
;; We will have to work to make the bugs show, but they are there.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There are some problems that one runs into when constructing macros in this
;; way, and they are problems that all lisps have to find ways of solving.

;; I don't think people realize quite how clever clojure's namespace and
;; backquote system are. They make what were serious problems with nasty
;; solutions in traditional lisps into minor difficulties in clojure.

;; But we should understand the problems, in order to understand the answer, and
;; use clojure's macros with confidence.

;; Let's have a closer look at our loop macro:

(defmacro fori-bugs-1 [finish & code]
  (list 'loop '[i 0]
        (concat
         (concat (list 'when) (list (list '<= 'i finish))
                 code)
         (list (list 'recur '(inc i))))))


;; Now this macro, simple though it is, is sufficiently complex that it runs
;; into all the traditional difficulties of macros:

;; The first difficulty is that the functions the macro uses or expands into may
;; not be the same when it is expanded as they were when it was defined.

;; Suppose I have innocently redefined concat.
(defn concat [list1 list2]
  (if (empty? list1) list2
      (cons (first list1) (concat (rest list1) list2))))

;; Clojure sweetly warns me that I am redefining a variable that is already present in this namespace:
;; WARNING: concat already refers to: #'clojure.core/concat in namespace: user, being replaced by: #'user/concat

;; This traditional definition works fine, modulo blowing stack
(concat '(a b) '(c d))

;; But our macro is relying on the more general version in clojure/core
(fori-bugs-1 10 (print i))
;; Wrong number of args (3) passed to: user$concat

;; Now you may think that anyone who redefines concat deserves anything they
;; get, but this would put an intolerable burden on programmers. There are many
;; functions in core and contrib. Are we supposed to avoid them all just in case
;; one of them is being used by a macro?

;; This is exactly the problem that namespaces are supposed to solve. I have
;; defined user/concat, and my macro would like to use clojure.core/concat.

;; So let's redefine fori to be proof against the sort of halfwits who redefine
;; concat:
(defmacro fori-bugs-2 [finish & code]
  (list 'loop '[i 0]
        (clojure.core/concat
         (clojure.core/concat
          (list 'when) (list (list '<= 'i finish))
          code)
         (list (list 'recur '(inc i))))))


;; All is well 
(fori-bugs-2 10 (print i))
;;012345678910nil

;; But notice that someone could redefine list, or inc, or when, or even <=.
;; The one thing they can't redefine is recur, because that isn't a function
;; with a namespace, but one of clojure's few non-negotiable keywords.

;; This might throw some light on syntax-quote's behaviour here:
`(concat list when recur <=)
;;(user/concat clojure.core/list clojure.core/when recur clojure.core/<=)
;; It makes it much easier for us to ensure that the functions that a macro uses
;; are the same functions that the macro writer thought they were.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The second traditional difficulty of macros is that control of evaluation
;; has given us enough rope to shoot ourselves in the foot.

;; Consider:
(fori-bugs-2 (rand 10) (print i))
;; 012nil
;; 0123nil
;; 01nil

;; Looks ok to me. But how many times is rand getting called?

;; We can use dbg to find out:
(fori-bugs-2 (dbg (rand 10)) (print i))
;; (rand 10) = 3.9962555466190786
;; 0(rand 10) = 1.0304724843938684
;; 1(rand 10) = 7.53583399884498
;; 2(rand 10) = 7.749905450994815
;; 3(rand 10) = 9.867298137476428
;; 4(rand 10) = 5.260154505531479
;; 5(rand 10) = 6.552723147015951
;; 6(rand 10) = 4.978421104503629
;; nil

;; Or we could use the trace function from contrib
(require 'clojure.contrib.trace)
(clojure.contrib.trace/dotrace (rand) (fori-bugs-2 (rand 10) (print i)))

;;TRACE t12262: (rand 10)
;;TRACE t12262: => 7.637716578809059
;;0TRACE t12263: (rand 10)
;;TRACE t12263: => 1.7129420087274194
;;1TRACE t12264: (rand 10)
;;TRACE t12264: => 9.701793250880767
;;2TRACE t12265: (rand 10)
;;TRACE t12265: => 1.4731346458223638
;;nil

;; Was that what you expected? It looks as though we've violated the principle
;; of least surprise.

;; What is going on, and how to fix it?

;; What does the macro expand into?
(macroexpand-1 '(fori-bugs-2 (rand 10) (print i)))
;; goes to:
(loop [i 0]
  (when (<= i (rand 10))
    (print i)
    (recur (inc i))))
;; Do you see the problem?

;; What we really wanted was:
(let [finish (rand 10)]
  (loop [i 0]
    (when (<= i finish)
      (print i)
      (recur (inc i)))))
;; Which evaluates the finish condition once, and then uses that value
;; thereafter.  An apprentice programmer might have noticed this (although no
;; guarantees, in my experience.) But a compiler always does exactly what it's
;; told, if it can.

;; Let's change our macro so it does what we really want
(defmacro fori-3-bugs [end & code]
  (list 'let ['finish end]
        (list 'loop '[i 0]
              (clojure.core/concat
               (clojure.core/concat
                (list 'when) (list (list '<= 'i 'finish))
                code)
               (list (list 'recur '(inc i)))))))


(macroexpand-1 '(fori-3-bugs (rand 10) (print i)))
;; becomes
(let [finish (rand 10)]
  (loop [i 0]
    (when (<= i finish)
      (print i)
      (recur (inc i)))))

(fori-3-bugs (rand 10) (print i))
;; 01nil
;; 012345678nil

(fori-3-bugs (dbg (rand 10)) (print i))
;; (rand 10) = 8.582591932245174
;; 012345678nil

(dotrace (rand) (fori-3-bugs (rand 10) (print i)))
;;TRACE t12531: (rand 10)
;;TRACE t12531: => 6.961169251276846
;;0123456nil

;; All is well. Except for....

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The third traditional difficulty of macros is conflict between temporary
;; variables introduced in the macro and variables in the surrounding scope.

;; Now that we have introduced a temporary variable, in order to avoid problem
;; two, We are vulnerable to this bug as well.

;; Consider:

(let [cutoff 4]
  (fori-3-bugs 10 (when (<= i cutoff) (print i))))
;; 01234nil

;; So far so good. But what if we use a different name for our cutoff?

(let [finish 4]
  (fori-3-bugs 10 (when (<= i finish) (print i))))
;; 012345678910nil

;; Bother.

;; If that doesn't violate the principle of least surprise, I don't know what does!

;; Let us again examine our expansion:
(macroexpand-1 '(fori-3-bugs 10 (when (<= i finish) (print i))))

(let [finish 10]
  (loop [i 0]
    (when (<= i finish)
      (when (<= i finish) (print i))
      (recur (inc i)))))

;; It's pretty clear what the problem is. We should choose a less obvious name
;; for our temporary variable finish.

;; But what to choose? 

;; Clojure gives us the function gensym, which every time it's called gives us
;; back a different unlikely symbol.

(gensym) ;; G__11108
(gensym) ;; G__11113


;; So we can use that:

(defmacro fori-4-bugs [end & code]
  (let [finish (gensym)]
    (list 'let [finish end]
          (list 'loop '[i 0]
                (clojure.core/concat
                 (clojure.core/concat
                  (list 'when) (list (list '<= 'i finish))
                  code)
                 (list (list 'recur '(inc i))))))))

;; finish is now just a variable local to the code-generating process. In the
;; expanded code, there will instead be an unlikely symbol which won't be the
;; same as any other symbols generated by gensym, and is unlikely to be the same
;; as any symbol we're using unless we're playing silly buggers.

(let [finish 4]
  (fori-4-bugs 10 (when (<= i finish) (print i))))
;; 01234nil
;; 01234nil

(let [finish 4]
  (fori-4-bugs (rand 10) (when (<= i finish) (print i))))

;; 01234nil
;; 0nil
;; 01234nil
;; 0123nil

;; Joy!


;; Let's have a look what this expands to:
(macroexpand-1 '(fori-4-bugs (rand 10) (when (<= i finish) (print i))))
(let [G__11183 (rand 10)]
  (loop [i 0]
    (when (<= i G__11183)
      (when (<= i finish) (print i))
      (recur (inc i)))))

;; Notice that the variable finish in the code passed in hasn't been messed about with.
;; It is the temporary variable introduced in the macro to control the evaluation of (rand 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So why is it still called fori-4-bugs?

;; Well I'm just yanking your chain. The only thing that's wrong is that we
;; still haven't namespace-qualified all the functions.

;; Actually, there aren't any more problems to deal with after that. 

(defmacro fori-5 [end & code]
  (let [finish (gensym)]
    (clojure.core/list 'clojure.core/let [finish end]
       (clojure.core/list 'clojure.core/loop '[i 0]
         (clojure.core/concat
           (clojure.core/concat
             (clojure.core/list 'clojure.core/when) (clojure.core/list (clojure.core/list 'clojure.core/<= 'i finish))
               code)
           (clojure.core/list (clojure.core/list 'recur '(clojure.core/inc i))))))))

(let [finish 7]
  (fori-5 (rand 10) (when (<= i finish) (print i))))
;;01234567nil
;;012nil
;;01234567nil
;;01234nil


;; Let's have a look at the final expansion:
(macroexpand-1 '(fori-5 (rand 10) (when (<= i finish) (print i))))

(clojure.core/let [G__11576 (rand 10)]
                  (clojure.core/loop [i 0]
                                     (clojure.core/when (clojure.core/<= i G__11576)
                                                        (when (<= i finish) (print i))
                                                        (recur (clojure.core/inc i)))))

;; Unfortunately, our macro, now bulletproof, has become a gigantic unreadable
;; pain in the ass.

;; Such code would be hard to get right, fiendishly hard to understand, and
;; downright impossible to debug.

;; So all lisps find ways of making the process easier.

;; Clojure's is:

(defmacro fori [end & code]
  `(let [finish# ~end]
     (loop [~'i 0]
       (when (<= ~'i finish#)
         ~@code
         (recur (inc ~'i))))))


;; The original problem was to make a macro such that 
(fori 10 (print i))
;; would turn into:
(loop [i 0]
  (when (<= i 10)
    (print i)
    (recur (inc i))))

;; The solution above, using the backquotes, looks very like this, except for
;; the introduction of the temporary variable finish, and a lot of funny #,~,~@,
;; and ~' characters

;; Let's look at how the expansion of this final version looks when we try it on
;; the hard case before:

(macroexpand-1 '(fori (rand 10) (when (<= i finish) (print i))))

(clojure.core/let [finish__11689__auto__ (rand 10)]
                  (clojure.core/loop [i 0]
                                     (clojure.core/when
                                      (clojure.core/<= i finish__11689__auto__)
                                      (when (<= i finish) (print i))
                                      (recur (clojure.core/inc i)))))

;; It's pretty much exactly how it was when it was being made by a lot of
;; unreadable rubbish.

;; The only difference is that the gensym name is different. When you get
;; syntax-quote to make gensyms for you, they have a prefix name that is like
;; the one you typed (here finish), and a suffix __auto__ to let you know that
;; you're looking at a gensym that came from finish#, rather than one that was
;; made with (gensym "finish__")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; So what are the rules for making code transformations into macros?

;; Write down what you want the code to look like before expansion
(fori (rand 10) (print i))

;; Write down what you want the code to look like after expansion.
;; Remember to control the evaluation of the parameters properly.
;; Giving you control over this is what macros are for.
(let [finish (rand 10)]
     (loop [i 0]
       (when (<= i finish)
         (print i)
         (recur (inc i)))))

;; Take that skeleton, and make it into a function which gives back code
;; by using the backquote operator, and the strange characters ~, ~@, # and ~'
(defn fori-fn [end & code]
  `(let [finish# ~end]
     (loop [i 0]
       (when (<= i finish#)
         ~@code
         (recur (inc i))))))

;; ~  is for replacing parameters

;; ~@ is also for replacing parameters,
;; but it splices a list into the surrounding list, removing the outside brackets.

;; #  is for creating variables with gensym names,
;; so as not to capture variables in the outside scope.

;; ~' is for creating names which do capture variables in the outside scope.

;; And that's pretty much it for the syntax-quote syntax. It does a lot of
;; tedious work for you if you know how to use it.













































 















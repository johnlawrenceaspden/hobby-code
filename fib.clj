;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trampolining Your Way Around A Tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I'm going to try to convert some other recursive algorithms to run in clojure
;; without smashing the JVM's stack

;; Here's the classic tree recursion for fib 

(defn fib[n]
  (cond (= n 0) 0N
        (= n 1) 1N
        :else (+ (fib (dec n)) (fib (dec (dec n))))))

;; Note the 0N and 1N to cause bignum contagion in clojure 1.3.0

;; It's slow:
(time (fib 30)) ; "Elapsed time: 1756.324843 msecs"
;; 832040N

;; But it benefits greatly from memoization:
(def fib (memoize fib))
(time (fib 30)) ; "Elapsed time: 0.117492 msecs"
;; 832040N

;; Does anybody know why this has started working again? In clojure 1.2 I needed
;; to make the recursive calls with #' in order to get the memoization benefit,
;; but it's now just working (in 1.3.0-alpha3)

;; And here's how to do it with the make-your-own-stack technique:

;; Anybody know what this is called? It reminds me of trampolining, but I can't
;; get that to work with non-tail recursions.  I'm using the variant with code
;; generation and eval because it's easy to debug.

;; Here's all the machinery from the previous post. I won't explain it again.

(def  fact-list (atom {}))
(defn add-fact! [n fn] (swap! fact-list #(assoc % n fn)))

(def  to-do-list (atom '()))
(defn add-task! [t] (swap! to-do-list #(cons t %)))
(defn add-tasks! [tlist] (doseq [t (reverse tlist)] (add-task! t)))
(defn pop-task! [] (let [h (first @to-do-list)] (swap! to-do-list rest) h))
(defn run-list! []
  (let [a (pop-task!)]
    (when (not (nil? a))
      (eval a)
      (recur))))

(defn peek-lists [] [fact-list to-do-list])
(defn init! [] (reset! fact-list {}) (reset! to-do-list '()))

(defn calculate-fib[n]
  (init!)
  (let [a (fib n)]
    (if (= a :tasks-added-to-do-list)
      (do 
        (run-list!)
        (fib n))
      a)))

;; And here is the fib function itself. It looks very complicated compared to
;; the version above, but it really is running the same memoized tree recursion.

(defn fib[n]
  (let [return (fn[x] (add-fact! n x) x)]  ;; local function to remember returned values
    (cond  (= n 0) (return 0N)             ;; base cases as above
           (= n 1) (return 1N)
           :else (let [fdn (@fact-list (dec n))   ;; but if we need to recurse
                       fddn (@fact-list (dec (dec n)))] ;;check that the prerequisites have already been calculated
                   (if (and fdn fddn)                   ;; and if they have 
                     (return (+ fdn fddn))              ;; calculate as above
                     (do                                ;; but if not
                       (add-task! (list 'fib n))                    ;; re-queue this task
                       (when (nil? fdn) (add-task! (list 'fib (dec n)))) ;; to be done after whichever of the two prerequisites 
                       (when (nil? fddn) (add-task! (list 'fib (dec (dec n))))) ;; need doing first
                        :tasks-added-to-do-list))))))

(time (calculate-fib 30)) ; "Elapsed time: 94.69365 msecs"
832040N
 
;; We can watch what goes on here, by running the recursion by hand.

(init!) ; ()
(peek-lists) ; [#<Atom@1c026b2: {}> #<Atom@10d6e40: ()>]
(fib 5) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@1c026b2: {}> #<Atom@10d6e40: ((fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@1c026b2: {}> #<Atom@10d6e40: ((fib 1) (fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 1N
(peek-lists) ; [#<Atom@1c026b2: {1 1N}> #<Atom@10d6e40: ((fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@1c026b2: {1 1N}> #<Atom@10d6e40: ((fib 0) (fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 0N
(peek-lists) ; [#<Atom@1c026b2: {0 0N, 1 1N}> #<Atom@10d6e40: ((fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 1N
(peek-lists) ; [#<Atom@1c026b2: {2 1N, 0 0N, 1 1N}> #<Atom@10d6e40: ((fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 2N
(peek-lists) ; [#<Atom@1c026b2: {3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@10d6e40: ((fib 4) (fib 5))>]
(eval (pop-task!)) ; 3N
(peek-lists) ; [#<Atom@1c026b2: {4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@10d6e40: ((fib 5))>]
(eval (pop-task!)) ; 5N
(peek-lists) ; [#<Atom@1c026b2: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@10d6e40: ()>]
(eval (pop-task!)) ; nil
(peek-lists) ; [#<Atom@1c026b2: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@10d6e40: ()>]
(eval (pop-task!)) ; nil
(peek-lists) ; [#<Atom@1c026b2: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@10d6e40: ()>]
(eval (pop-task!)) ; nil
(peek-lists) ; [#<Atom@1c026b2: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@10d6e40: ()>]
(eval (pop-task!)) ; nil
(peek-lists) ; [#<Atom@1c026b2: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@10d6e40: ()>]
(eval (pop-task!)) ; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; And of course, using eval at run-time is slow, so instead we can redefine the
;; task runner and the fib function so:

(defn run-list! []
  (let [a (pop-task!)]
    (when (not (nil? a))
      (a)
      (recur))))

(defn fib[n]
  (let [return (fn[x] (add-fact! n x) x)]
    (if (< n 2) (return (bigint n))
        (let [fdn (@fact-list (dec n))
              fddn (@fact-list (dec (dec n)))]
          (if (and fdn fddn)
            (return (+ fdn fddn))
            (do (add-tasks! (list #(fib (dec (dec n))) #(fib (dec n)) #(fib n)))
                :tasks-added-to-do-list))))))

;; Which again gives us a 100x speed up, at the cost of not being so easy to
;; understand using peek-lists:

(time (calculate-fib 30)) ; "Elapsed time: 0.864894 msecs"
;; 832040

;; This time, the make-your-own-stack version is considerably slower than the
;; natural version.  (By about a factor of 8).

;; Which is as it should be!

;; But we have a non-stack-blowing program
(time (calculate-fib 300)) ; "Elapsed time: 2.953718 msecs"
;; 222232244629420445529739893461909967206666939096499764990979600N

(time (calculate-fib 5000)) ; "Elapsed time: 64.006783 msecs"
;; 38789684543883256337019.......4382863125N

(time (calculate-fib 50000)) ; "Elapsed time: 2950.429377 msecs"
;; 10777734893072974780279...18305364252373553125N

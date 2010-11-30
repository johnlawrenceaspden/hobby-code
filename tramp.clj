;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yet Another Way To Write Factorial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Suppose I want to perform a recursive algorithm in Clojure:

(defn fact[n]
  (if (< n 2) 1N (* n (fact (dec n)))))

(fact 5)    ; 120N
(fact 5000) ; blows stack

;; Note that this is nothing to do with the lack of tail-call optimization in Java.

;; The problem here is that there is a hard limit on the size of the stack, which is set too low.

;; What are my options?

;; I can transform the algorithm into an iteration, which can be expressed nicely in clojure using recur.

(defn fact
  ([n acc] (if (< n 2) acc (recur (dec n) (* acc n))))
  ([n] (fact n 1N)))

(time (fact 5000)) "Elapsed time: 273.485278 msecs"

;; But although this is easy enough in this case, it won't always be possible.

;; Or I can keep the list of tasks to do somewhere other than the JVM's tiny stack.

;; Let's look at the second option:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The first place I'm going to keep the stack is in my head. 

;; I'm going to write a function which either tells me the answer, or tells me what to type at the repl to get the answer

(defn fact[n]
  (if (< n 2) 1N
      (str "calculate (fact " (dec n) ") then you can calculate (fact " n ") with (* " n " (fact " (dec n)"))" )))

(fact 5) ; "calculate (fact 4) then you can calculate (fact 5) with (* 5 (fact 4))"
(fact 4) ; "calculate (fact 3) then you can calculate (fact 4) with (* 4 (fact 3))"
(fact 3) ; "calculate (fact 2) then you can calculate (fact 3) with (* 3 (fact 2))"
(fact 2) ; "calculate (fact 1) then you can calculate (fact 2) with (* 2 (fact 1))"
(fact 1) ; 1N
(* 2 1N) ; 2N
(* 3 2N) ; 6N
(* 4 6N) ; 24N
(* 5 24N) ; 120N

;; The amount of typing and remembering what to do here seems excessive, but at least we have an algorithm that will not break Java!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's see how much of this work we can offload onto the computer:
;; The most annoying thing when doing the procedure above is substituting 6N for (fact 3)

;; so let's make a table for those values, and get the fact function to fill it in for us
(def fact-list (atom {}))
(defn add-fact! [n fn] (swap! fact-list #(assoc % n fn)))

(defn fact[n]
  (let [return (fn[x] (add-fact! n x) x)]
    (if (< n 2) (return 1N)
        (if-let [fdn (@fact-list (dec n))] (return (* n fdn))
                (str "calculate (fact " (dec n) ") then you can calculate (fact " n ")" )))))


(fact 5) ; "calculate (fact 4) then you can calculate (fact 5)"
(fact 4) ; "calculate (fact 3) then you can calculate (fact 4)"
(fact 3) ; "calculate (fact 2) then you can calculate (fact 3)"
(fact 2) ; "calculate (fact 1) then you can calculate (fact 2)"
(fact 1) ; 1N
(fact 2) ; 2N
(fact 3) ; 6N
(fact 4) ; 24N
(fact 5) ; 120N

;; Now the most troublesome task is remembering the stack of functions to call.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def to-do-list (atom '()))
(defn add-task![t] (swap! to-do-list #(cons t %)))
(defn add-tasks![tlist] (doseq [t (reverse tlist)] (add-task! t)))
(defn pop-task![] (let [h (first @to-do-list)] (swap! to-do-list rest) h))

(defn init! [] (reset! fact-list {}) (reset! to-do-list '()))

(defn fact[n]
  (let [return (fn[x] (add-fact! n x) x)]
    (if (< n 2) (return 1N)
        (if-let [fdn (@fact-list (dec n))] (return (* n fdn))
                (do (add-tasks! (list (list 'fact (dec n)) (list 'fact n)))
                    :tasks-added-to-do-list)))))



;; Now the computer will keep track of what to do next for us.
;; We just execute whatever pop-task tells us to.
        
(init!) ; ()
(fact 5) ; :tasks-added-to-do-list
(pop-task!) ; (fact 4)
(fact 4) ; :tasks-added-to-do-list
(pop-task!) ; (fact 3)
(fact 3) ; :tasks-added-to-do-list
(pop-task!) ; (fact 2)
(fact 2) ; :tasks-added-to-do-list
(pop-task!) ; (fact 1)
(fact 1) ; 1N
(pop-task!) ; (fact 2)
(fact 2) ; 2N
(pop-task!) ; (fact 3)
(fact 3) ; 6N
(pop-task!) ; (fact 4)
(fact 4) ; 24N
(pop-task!) ; (fact 5)
(fact 5) ; 120N
(pop-task!) ; nil

;; Once we've run out of tasks, we can re-ask the original question
(fact 5) ; 120N

;; But of course we could just use eval to execute the code returned by pop-task.

(init!) ; ()
(fact 5) ; :tasks-added-to-do-list
(eval (pop-task!)) ; :tasks-added-to-do-list
(eval (pop-task!)) ; :tasks-added-to-do-list
(eval (pop-task!)) ; :tasks-added-to-do-list
(eval (pop-task!)) ; 1N
(eval (pop-task!)) ; 2N
(eval (pop-task!)) ; 6N
(eval (pop-task!)) ; 24N
(eval (pop-task!)) ; 120N
(eval (pop-task!)) ; nil

;; Once pop-task! returns nil, we're done, and we can ask our original question.
(fact 5) ; 120N


;; And repeatedly typing (eval (pop-task!)) is a bit of a pain
(defn run-list []
  (let [a (eval (pop-task!))]
    (when (not (nil? a))
      (recur)))) 


(init!) ; ()
(fact 5) ; :tasks-added-to-do-list
(run-list) ; nil
(fact 5) ; 120N

;; And now we can calculate 5000! without blowing the stack or wearing our fingers to the bone

(init!) ; ()
(fact 5000) ; :tasks-added-to-do-list
(run-list) ; nil
(fact 5000) ; 4228577..........0000000000000000N

;; The whole thing can be summed up as:

(defn calculate-fact[n]
  (init!)
  (let [a (fact n)]
    (if (= a :tasks-added-to-do-list)
      (do 
        (run-list)
        (fact n))
      a)))

(calculate-fact 5) ; 120N
(calculate-fact 50) ; 30414093201713378043612608166064768844377641568960512000000000000N
(calculate-fact 500) ; 12201......0000000000000000000000000000000000000N


;; But it is slow:

(time (calculate-fact 5000)) "Elapsed time: 31004.287913 msecs"

;; We can recover the speed by keeping actual functions on the list rather than code, and executing them
;; instead of evaluating the code.

(defn fact[n]
  (let [return (fn[x] (add-fact! n x) x)]
    (if (< n 2) (return 1N)
        (if-let [fdn (@fact-list (dec n))] (return (* n fdn))
                (do (add-tasks! (list #(fact (dec n)) #(fact n)))
                    :tasks-added-to-do-list)))))


(defn run-list []
  (let [a (pop-task!)]
    (when (not (nil? a))
      (a)
      (recur))))


(calculate-fact 5000) ; 422800000....................00000000000N
(time (calculate-fact 5000))  "Elapsed time: 219.510832 msecs"


;; I'm surprised to find that this seems to be slightly faster than the tail-call version.
;; I wasn't expecting that at all, and wonder if I've just made some ghastly mistake.
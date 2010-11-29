(defn fib[n]
  (cond (= n 0) 0N
        (= n 1) 1N
        :else (+ (fib (dec n)) (fib (dec (dec n))))))

(time (fib 30)) ; "Elapsed time: 1756.324843 msecs"
;;832040N

(def fib (memoize fib))
(time (fib 30)) ; "Elapsed time: 1756.324843 msecs"




(def fact-list (atom {}))
(defn add-fact! [n fn] (swap! fact-list #(assoc % n fn)))

(def to-do-list (atom '()))
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




(time (calculate-fib 30)) ; "Elapsed time: 0.459898 msecs"
832040N 

(defn fib[n]
  (let [return (fn[x] (add-fact! n x) x)]
    (cond  (= n 0) (return 0N)
           (= n 1) (return 1N)
           :else (let [fdn (@fact-list (dec n))
                       fddn (@fact-list (dec (dec n)))]
                   (if (and fdn fddn)
                     (return (+ fdn fddn))
                     (do (add-tasks! (list (list 'fib (dec (dec n))) (list 'fib (dec n)) (list 'fib n)))
                         :tasks-added-to-do-list))))))

(init!) ; ()
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ()>]
(fib 5) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ((fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ((fib 1) (fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 1N
(peek-lists) ; [#<Atom@c2e1b1: {1 1N}> #<Atom@6f4165: ((fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@c2e1b1: {1 1N}> #<Atom@6f4165: ((fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 0N
(peek-lists) ; [#<Atom@c2e1b1: {0 0N, 1 1N}> #<Atom@6f4165: ((fib 1) (fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 1N
(peek-lists) ; [#<Atom@c2e1b1: {2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 2N
(peek-lists) ; [#<Atom@c2e1b1: {3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 4) (fib 5))>]
(eval (pop-task!)) ; 3N
(peek-lists) ; [#<Atom@c2e1b1: {4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 5))>]
(eval (pop-task!)) ; 5N
(peek-lists) ; [#<Atom@c2e1b1: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ()>]
(eval (pop-task!)) ; nil
(peek-lists) ; [#<Atom@c2e1b1: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ()>]
(eval (pop-task!)) ; nil
(peek-lists) ; [#<Atom@c2e1b1: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ()>]
(eval (pop-task!)) ; nil
(peek-lists) ; [#<Atom@c2e1b1: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ()>]
(eval (pop-task!)) ; nil
(peek-lists) ; [#<Atom@c2e1b1: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ()>]
(eval (pop-task!)) ; nil


(defn fib[n]
  (let [return (fn[x] (add-fact! n x) x)]
    (cond  (= n 0) (return 0N)
           (= n 1) (return 1N)
           :else (let [fdn (@fact-list (dec n))
                       fddn (@fact-list (dec (dec n)))]
                   (if (and fdn fddn)
                     (return (+ fdn fddn))
                     (do (add-tasks! (list (list 'fib (dec n)) (list 'fib (dec (dec n))) (list 'fib n)))
                         :tasks-added-to-do-list))))))

(init!) ; ()
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ()>]
(fib 5) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ((fib 4) (fib 3) (fib 5))>]
(eval (pop-task!)) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ((fib 3) (fib 2) (fib 4) (fib 3) (fib 5))>]
(eval (pop-task!)) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ((fib 2) (fib 1) (fib 3) (fib 2) (fib 4) (fib 3) (fib 5))>]
(eval (pop-task!)) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ((fib 1) (fib 0) (fib 2) (fib 1) (fib 3) (fib 2) (fib 4) (fib 3) (fib 5))>]
(eval (pop-task!)) ; 1N
(peek-lists) ; [#<Atom@c2e1b1: {1 1N}> #<Atom@6f4165: ((fib 0) (fib 2) (fib 1) (fib 3) (fib 2) (fib 4) (fib 3) (fib 5))>]
(eval (pop-task!)) ; 0N
(peek-lists) ; [#<Atom@c2e1b1: {0 0N, 1 1N}> #<Atom@6f4165: ((fib 2) (fib 1) (fib 3) (fib 2) (fib 4) (fib 3) (fib 5))>]
(eval (pop-task!)) ; 1N
(peek-lists) ; [#<Atom@c2e1b1: {2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 1) (fib 3) (fib 2) (fib 4) (fib 3) (fib 5))>]
(eval (pop-task!)) ; 1N
(peek-lists) ; [#<Atom@c2e1b1: {2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 3) (fib 2) (fib 4) (fib 3) (fib 5))>]
(eval (pop-task!)) ; 2N
(peek-lists) ; [#<Atom@c2e1b1: {3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 2) (fib 4) (fib 3) (fib 5))>]
(eval (pop-task!)) ; 1N
(peek-lists) ; [#<Atom@c2e1b1: {3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 4) (fib 3) (fib 5))>]
(eval (pop-task!)) ; 3N
(peek-lists) ; [#<Atom@c2e1b1: {4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 3) (fib 5))>]
(eval (pop-task!)) ; 2N
(peek-lists) ; [#<Atom@c2e1b1: {4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 5))>]
(eval (pop-task!)) ; 5N
(peek-lists) ; [#<Atom@c2e1b1: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ()>]
(eval (pop-task!)) ; nil








(time (calculate-fib 300)) ; 

(time (calculate-fib 2))

(defn fib[n]
  (let [return (fn[x] (add-fact! n x) x)]
    (cond  (= n 0) (return 0N)
           (= n 1) (return 1N)
           :else (let [fdn (@fact-list (dec n))
                       fddn (@fact-list (dec (dec n)))]
                   (if (and fdn fddn)
                     (return (+ fdn fddn))
                     (do
                       (add-task! (list 'fib n))
                       (when (nil? fdn) (add-task! (list 'fib (dec n))))
                       (when (nil? fddn) (add-task! (list 'fib (dec (dec n)))))
                        :tasks-added-to-do-list))))))


(init!) ; ()
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ()>]
(fib 5) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ((fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@c2e1b1: {}> #<Atom@6f4165: ((fib 1) (fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 1N
(peek-lists) ; [#<Atom@c2e1b1: {1 1N}> #<Atom@6f4165: ((fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; :tasks-added-to-do-list
(peek-lists) ; [#<Atom@c2e1b1: {1 1N}> #<Atom@6f4165: ((fib 0) (fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 0N
(peek-lists) ; [#<Atom@c2e1b1: {0 0N, 1 1N}> #<Atom@6f4165: ((fib 2) (fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 1N
(peek-lists) ; [#<Atom@c2e1b1: {2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 3) (fib 4) (fib 5))>]
(eval (pop-task!)) ; 2N
(peek-lists) ; [#<Atom@c2e1b1: {3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 4) (fib 5))>]
(eval (pop-task!)) ; 3N
(peek-lists) ; [#<Atom@c2e1b1: {4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ((fib 5))>]
(eval (pop-task!)) ; 5N
(peek-lists) ; [#<Atom@c2e1b1: {5 5N, 4 3N, 3 2N, 2 1N, 0 0N, 1 1N}> #<Atom@6f4165: ()>]
(eval (pop-task!)) ; nil


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn run-list! []
  (let [a (pop-task!)]
    (when (not (nil? a))
      (a)
      (recur))))


(defn fib[n]
  (let [return (fn[x] (add-fact! n x) x)]
    (if (< n 2) (return n)
        (let [fdn (@fact-list (dec n))
              fddn (@fact-list (dec (dec n)))]
          (if (and fdn fddn)
            (return (+ fdn fddn))
            (do (add-tasks! (list #(fib (dec (dec n))) #(fib (dec n)) #(fib n)))
                :tasks-added-to-do-list))))))

(time (calculate-fib 300))
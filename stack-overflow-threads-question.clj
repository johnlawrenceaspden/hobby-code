(defn fib [n] 
   (if  (or (zero? n) (= n 1)) 
       1 
       (+  (fib (dec n) )  (fib (- n 2)))))

(defn calc! [r counter-A counter-B counter-C n]
  (dosync
   (swap! counter-A inc)
   ;;(Thread/sleep n)
   (fib n)
   (swap! counter-B inc)
   (alter r inc)
   (swap! counter-C inc)))

(def r (ref 0))
(def counter-A (atom 0))
(def counter-B (atom 0))
(def counter-C (atom 0))

(defn report []
  (println "r:" @r ", A:" @counter-A ", B:" @counter-B ", C:" @counter-C))

(defn main [thread-num n]
    (doall (pmap deref
                 (for [_ (take thread-num (repeat nil))]
                   (future (calc! r counter-A counter-B counter-C n)))))
)
        
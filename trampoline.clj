;;measuring the stack with bouncing bombs

(defn bounce [fn]
  (print "bounce")
  (let [[callagain result] (fn)]
    (if callagain (bounce result)
        (do (println)
            result))))
        

(bounce (fn [] [false "doom"]))
(bounce (fn [] [true (fn[] [false "doom"])]))

(def a (fn[] [false "doom"]))

(bounce a)

(defn wrap [f]
     (fn[] [true f]))

(bounce (wrap (wrap a)))

(defn bomb []
  [true, (fn[] (bomb))])

(bounce bomb)

(defn countingbomb 
  ([] [true (fn[] (countingbomb 0))])
  ([n] [true (fn[] (countingbomb (inc n)))]))

(bounce countingbomb)
  
(defn printingbomb 
  ([] [true (fn[] (printingbomb 0))])
  ([n] [true (fn[] (do
                     (println n)
                     (printingbomb (inc n))))]))

(bounce printingbomb)

(defn bounce-of-death [fn]
  (print "bounce!")
  (let [[callagain result] (fn)]
    (if callagain (recur result)
        (do (println)
            result))))

(bounce-of-death printingbomb)

(declare ping pong)

(defn ping
  ([] [true (fn[] (println "ping" 0)(pong 1))])
  ([n] [true (fn[] (println "ping" n)(pong (+ 1 n)))]))

(defn pong
  ([n] [true (fn[] (println "pong" n)(ping (+ n 1)))]))

(bounce ping)

(defn continue-with
  ([cont] (fn[] [true (fn[] (cont 1))])))
  
(bounce (continue-with print))
(bounce (continue-with (fn[x] [false x])))

(defn identity-cont
  ([n cont] (fn[] [true (fn[] (cont n))])))

(bounce (identity-cont 10 (fn[x] [false x])))

(defn factorialbomb 
  ([]      [true (fn[] (factorialbomb 1 1))])
  ([acc n] [true (fn[] (do
                     (println n acc)
                     (factorialbomb (* acc n) (inc n))))]))


(bounce factorialbomb)

(defn print-zero
  ([] [true (fn[] (print 0))]))

(bounce (fn[] (print-zero)))

(defn func-zero
  ([func] [true (fn[] (func 0))]))

(bounce (fn[] (func-zero print)))
(bounce (fn[] (func-zero (fn[x][false x]))))


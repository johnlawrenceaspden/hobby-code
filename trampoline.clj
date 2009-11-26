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

(defn factorial [n c]
  (if (= n 0) (c 1)
      (fn [] (factorial (- n 1) (fn[result] (fn[] (c (* n result))))))))

(factorial 0 identity)
(identity 1)
1

(factorial 1 identity)
(fn[] (factorial 0 (fn[result] (fn[] (identity (* 1 result))))))

((factorial 1 identity))
(factorial 0 (fn[result] (fn[] (identity (* 1 result)))))
(fn[] (identity (* 1 1)))

(((factorial 1 identity)))
(identity (* 1 1))
1



(((factorial 1 identity)))
(((((factorial 2 identity)))))
(((((((factorial 3 identity)))))))

(defn t[f]
  (if (fn? f) (recur (f))
      f))

(t (factorial 10000 identity))



(defn factorial [n c]
  (if (= n 0) [false (c 1)]
      [true (fn [] (factorial (- n 1) (fn[result] [true (fn[] (c (* n result)))]))]))


(bounce (fn[] (factorial 0 identity) ))
(bounce (fn[] (factorial 1 identity) ))
(bounce-of-death (fn[] (factorial 5000 identity) ))


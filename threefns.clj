;;Three mutually recursive functions
;;assign! eliminate! and check!
;;operate by mutating a value v

(def v (atom 256))

(declare a e c)

(defn a[v]
  (print "a")
  (swap! v (fn[x] (+ (* 3 x) 1)))
  (if (= (mod @v 2) 0)
    (do
      (e v)
      (e v))
    v))

(defn e[v]
  (print "e")
  (when (= 0 (mod @v 2)) (swap! v (fn[x] (/ x 2))))
  (if (< (mod @v 3) 2)
    (do 
      (c v)
      (c v))
    v))

(defn c[v]
  (print "c")
  (swap! v (fn[x] (- x 1)))
  (if (< (mod @v 4) 2)
    (do 
      (a v)
      (a v))
    v))
           

(defn step[]
  (println v)
  (a v))
 

(swap! v (fn[x] 256) )

(step)
(step)
(step)
(step)
(step)
(step)
(step)
(step)

(println v)

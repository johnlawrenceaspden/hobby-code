; [(x,y,z) | x<- [1..100], y<-[1..100], z<-[1..100], x<y, x*x+y*y==z*z, (gcd (gcd x y ) z)==1]
; let gcd a b = (if a==0 then b else (if a<b then (gcd b a) else (gcd (a - b) b)))

(defn gcd [^long a ^long b] (if (zero? a) b (if (< a b) (recur b a) (recur (- a b) b))))

(gcd 27 18)

(time (doall (for [x (range 1 100)
                   y (range 1 100)
                   z (range 1 100) 
                   :when (< x y) 
                   :when (== (+ (* x x) (* y y)) (* z z))
                   :when (== 1 (gcd (gcd x y) z))]
               (list x,y,z))))

(time (doall (for [^long x (range 1 100)
                   ^long y (range 1 100)
                   ^long z (range 1 100) 
                   :when (< x y) 
                   :when (== (+ (* x x) (* y y)) (* z z))
                   :when (== 1 (gcd (gcd x y) z))]
               (list x,y,z))))

(time 
 ((fn [^long x ^long y ^long z ]
  (if (and (< x y)
           (== (+ (* x x) (* y y)) (* z z))
           (== 1 (gcd (gcd x y) z)))
    (println x y z))
  (if (< x 100) (recur (inc x) y z)
      (if (< y 100) (recur 0 (inc y) z)
          (if (< z 100) (recur 0 0 (inc z)))))) 0 0 0))
    
  







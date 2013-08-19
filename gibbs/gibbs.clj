;; import java.util.*;
;; import cern.jet.random.tdouble.*;
;; import cern.jet.random.tdouble.engine.*;
 
;; class Gibbs
;; {
 
;;     public static void main(String[] arg)
;;     {
;;     int N=50000;
;;     int thin=1000;
;;     DoubleRandomEngine rngEngine=new DoubleMersenneTwister(new Date());
;;     Normal rngN=new Normal(0.0,1.0,rngEngine);
;;     Gamma rngG=new Gamma(1.0,1.0,rngEngine);
;;     double x=0;
;;     double y=0;
;;     System.out.println("Iter x y");
;;     for (int i=0;i<N;i++) {
;;         for (int j=0;j<thin;j++) {
;;         x=rngG.nextDouble(3.0,y*y+4);
;;         y=rngN.nextDouble(1.0/(x+1),1.0/Math.sqrt(2*x+2));
;;         }
;;         System.out.println(i+" "+x+" "+y);
;;     }
;;     }
 
;; }

;; 2 mins and 27 secs on my netbook  (147 seconds) 
;; Article has this as 11 secs so ten times quicker!!

;; If we're interested in speed then this is always a cool thing to have on!
(set! *warn-on-reflection* true)

(cemerick.pomegranate/add-dependencies 
 :coordinates '[[net.sourceforge.parallelcolt/parallelcolt "0.10.0"]])
(import 'cern.jet.random.tdouble.engine.DoubleMersenneTwister)

(def N 50000)
(def thin 1000)

(def rngEngine (cern.jet.random.tdouble.engine.DoubleMersenneTwister. (java.util.Date.)))

(def rngN (cern.jet.random.tdouble.Normal. 0.0,1.0,rngEngine))
(def rngG (cern.jet.random.tdouble.Gamma.  1.0,1.0,rngEngine))

(def ^cern.jet.random.tdouble.Normal rngN (cern.jet.random.tdouble.Normal. 0.0,1.0,rngEngine))
(def ^cern.jet.random.tdouble.Gamma  rngG (cern.jet.random.tdouble.Gamma.  1.0,1.0,rngEngine))

;; Here's a straight translation. It is ridiculously slow!
(time (let [x (atom 0) y (atom 0)]
        (println "Iter x y")
        (loop [i 0]
          (loop [j 0]
            (swap! x (fn[_] (.nextDouble rngG 3.0 (+ (* @y @y) 4))))
            (swap! y (fn[_] (.nextDouble rngN (/ (+ 1 @x)) (/ 1.0 (Math/sqrt (+ 2 (* 2 @x)))))))
            (if (< j thin) (recur (inc j))))
          (if (zero? (mod i 1000)) (println i @x @y))
          (if (< i 500) (recur (inc i))))))




;; Drop the STM use!
(time (do (println "Iter x y")
          (loop [i 0 x 0 y 0]
            (let [[x y] (loop [j 0 x x y y]
                          (let [x (.nextDouble rngG 3.0 (+ (* y y) 4))
                                y (.nextDouble rngN (/ (+ 1 x)) (/ 1.0 (Math/sqrt (+ 2 (* 2 x)))))]
                            (if (< j thin) 
                              (recur (inc j) x y) 
                              [x y])))]
              (if (zero? (mod i 1000)) (println i x y))
              (if (< i N) (recur (inc i) x y))))))


;; Take out the inner loop
(defn iter [[x y]]
  (let [x (.nextDouble rngG 3.0 (+ (* y y) 4))
        y (.nextDouble rngN (/ (+ 1 x)) (/ 1.0 (Math/sqrt (+ 2 (* 2 x)))))]
    [x y]))

(defn thinner [[x y]] (nth (iterate iter [x y]) thin))

(time (count (doall (take 100 (take N (iterate thinner [0 0]))))))
;; 9 seconds, so 500*9 = 4500secs = 1 1/2 hours!


;; Let's try rolling up the inner loop, to avoid packing and unpacking

(defn thinner [[x y]]
  (loop [j 0 x x y y]
    (let [x (.nextDouble rngG 3.0 (+ (* y y) 4))
          y (.nextDouble rngN (/ (+ 1 x)) (/ 1.0 (Math/sqrt (+ 2 (* 2 x)))))]
      (if (< j thin) (recur (inc j) x y) [x y]))))

(time (count (doall (take 100 (take N (iterate thinner [0 0]))))))
;; 8.5 seconds

(defn thinner [[x y]]
  (loop [^long j thin 
         ^double x x 
         ^double y y 
         ^cern.jet.random.tdouble.Gamma  rngG rngG 
         ^cern.jet.random.tdouble.Normal rngN rngN]
    (let [x (.nextDouble rngG 3.0 (+ (* y y) 4))
          y (.nextDouble rngN (/ (+ 1 x)) (/ 1.0 (Math/sqrt (+ 2 (* 2 x)))))]
      (if (> j 0) (recur (dec j) x y rngG rngN)
          [x y]))))

(time (thinner [0 0]))
;;5.4 msecs

(time (count (doall (take 100 (take N (iterate thinner [0 0]))))))
;; 474ms (20x speedup) ;; so whole thing should be 250secs, roughly java speed.

(time (count (doall (take 1000 (take N (iterate thinner [0 0]))))))
;; 4323ms ;; so whole thing should be 215msecs

(time (count (doall (take 50000 (take N (iterate thinner [0 0]))))))
;; 228 seconds 

(time (count (doall (take 50000 (take N (iterate identity [0 0]))))))
;; 368msecs

(time (spit "delete.me" 
            (apply str 
                   (map (fn[[x y]] (str x "," y "\n"))
                        (take N (iterate (fn[[x y]] [(inc x)(inc y)]) [0 0]))))))
;; 2 secs

(time (spit "delete.me" 
            (apply str 
                   (map (fn[[x y]] (str x "," y "\n"))
                        (take N (iterate identity [0 0]))))))
;; 1.2secs


(time (spit "delete.me" 
            (apply str 
                   (map (fn[[x y]] (str x "," y "\n"))
                        (take N (iterate thinner [0 0]))))))
;; 267 secondss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn thinner [[x y]]
  (loop [j 0 x x y y]
    (let [x (.nextDouble ^cern.jet.random.tdouble.Gamma rngG 3.0 (+ (* y y) 4))
          y (.nextDouble ^cern.jet.random.tdouble.Normal rngN (/ (+ 1 x)) (/ 1.0 (Math/sqrt (+ 2 (* 2 x)))))]
      (if (< j thin) (recur (inc j) x y )
          [x y]))))

(time (thinner [0 0]))
;; 5.6msec

(time (count (doall (take 100 (take N (iterate thinner [0 0]))))))
;; 528 msec

(time (count (doall (take 1000 (take N (iterate thinner [0 0]))))))


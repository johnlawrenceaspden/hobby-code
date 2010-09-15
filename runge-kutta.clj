;; Numerical solution of dy/dt = f (t,y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optimization utilities


(set! *warn-on-reflection* true)

(require 'clojure.contrib.string)

(defn cpuspeed []
  "find out the cpu speed by reading /proc/cpuinfo"
  (/ (read-string
      (clojure.contrib.string/replace-by
       #"cpu MHz\t\t: " (constantly "")
       (first (clojure.contrib.string/grep
               #"cpu MHz.*"
               (line-seq
                (java.io.BufferedReader.
                 (java.io.FileReader. (java.io.File. "/proc/cpuinfo"))))))))
     1000.0))

(cpuspeed);; 2.399 GHz on desktop, 1.6 on laptop, but there are two cores there.
;; often when laptop is idle, the cpu will cut down to 0.8, which will screw things up fairly royally!
(defn cpuspeed[] 1.6) ;; fix the damned thing for laptop

(defmacro cyclesperit [expr its]
  `(let [start# (. System (nanoTime))
         ret# ( ~@expr ~its )
         finish# (. System (nanoTime))]
     (int (/ (* (cpuspeed) (- finish# start#)) ~its))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Runge-Kutta solver as originally written:

(defn f [t y] (- t y))

(defn solveit [t0 y0 h its]
  (if (> its 0) 
    (let [t1 (+ t0 h)
          y1 (+ y0 (* h (f t0 y0)))]
      (recur t1 y1 h (dec its)))
    [t0 y0 h its]))

;;desktop
3167
2704
3005
;;laptop seems to be faster per cycle. cpu usage is (100,0) in htop.
1927
1762
1765
(cyclesperit (solveit 0.0 1.0 0.01) 100000)


;; There's a fairly significant speed up to be had from
;; killing off function calls,
;; (I think because primitives don't make it through function boundaries)
;; We inline f and create an internal target for recur, with casts to doubles.

(defn solveit [t0 y0 h its]
  (loop [t0 (double t0) y0 (double y0) h (double h) its (int its)]
    (if (> its 0) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

;;desktop
570
580
564
;;laptop
384
384
386
(cyclesperit (solveit 0.0 1.0 0.01) 10000000)

;; profiler suggest integer casts done often

;; Another factor of ten comes from the (> its 0) call.
;; Even though it knows its is an int, it can't figure out that 0 is??
(defn solveit [t0 y0 h its]
  (loop [t0  (double t0)
         y0  (double y0)
         h   (double h)
         its (int its)]
    (if (> its (int 0)) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

;; desktop
60
63
61
;; laptop
50
50
49
(cyclesperit (solveit 0.0 1.0 0.01) 100000000)

;; The Lord alone knows what is going on here, for another factor of two
;; Making (int 0) into the explicit constant zero helps.

(defn solveit [t0 y0 h its]
  (let [zero (int 0)]
    (loop [t0 (double t0) y0 (double y0) h (double h) its (int its)]
      (if (> its zero) 
        (let [t1 (+ t0 h)
              y1 (+ y0 (* h (- t0 y0)))]
          (recur t1 y1 h (dec its)))
      [t0 y0 h its]))))

;;desktop
26
28
27
;;laptop

19
19
20
(cyclesperit (solveit 0.0  1.0 0.01) 100000000)

;; profiler says 2xadd, 1 gt, 1xminus, 1xdec 1xmulti
;; 4 floating point, 1 gt, 1 dec, 1 conditional branch;; 7 cycles/loop is optimal?
;; So maybe factor of 4 slower than hand-optimized?
;; Or factor of 3 on laptop!

(time (solveit 0.0 1.0 0.01 100000000))
;;laptop
"Elapsed time: 1215.044133 msecs"
[1000000.0007792843 999999.000779284 0.01 0]
;;desktop
"Elapsed time: 1130.129868 msecs"
[1000000.0007792843 999999.000779284 0.01 0]

;; 1.1 seconds to do 100 000 000 iterations on my desktop.
;; 1.2 seconds on my laptop, which has about half the clock speed.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy sequence thingy.
;; Assuming no way to optimize this because it has to involve function call?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn iterator [[t0 y0 h]]
  (let [t1 (+ t0 h)
        y1 (+ y0 (* h (f t0 y0)))]
    [t1 y1 h]))

(def iterations (iterate iterator [0.0 1.0 0.01]))

;;desktop
3271
3359
3727
;;laptop (again completely maxing out one core)
3232
3272
3234
(cyclesperit (nth iterations) 10000)


;; this doesn't seem to help
(defn iterator [[t0 y0 h]]
  (let [t0 (double t0)
        y0 (double y0)
        h  (double h)]
  (let [t1 (+ t0 h)
        y1 (+ y0 (* h (f t0 y0)))]
    [t1 y1 h])))

(def iterations (iterate iterator [0.0 1.0 0.01]))

;;desktop

3182
3463
3164
(cyclesperit (nth iterations) 10000)











;; Alternatively:
;;(time (nth iterations 100000))

;; On desktop: "Elapsed time: 619.078491 msecs"
;; 2 399 573 000 ops/sec
;; 2 399 573 ops/msec
;; 619 msecs / 100 000 its = 0.00619 msecs/it (* 0.00619 2399573)
;; 14853 cycles/it

;; On laptop:  "Elapsed time: 545.998816 msecs"
;; 1 600 000 000 ops/sec
;; 1 600 000 ops/msec
;; 545 msecs/ 100 000 its = 0.00545 msecs/it (* 0.00545 1600000)
;; 8720 ops/it??

;; ops/it=ghz*nanotime/its



;;(cyclesperit (nth iterations 100000) 100000)
;;desktop
;;[4841.019052760331 [999.9999999992356 998.9999999992357 0.01]]
;;[5454.73967420272 [999.9999999992356 998.9999999992357 0.01]]
;;laptop
;;[2880.876992 [999.9999999992356 998.9999999992357 0.01]]
;;[2918.492992 [999.9999999992356 998.9999999992357 0.01]]

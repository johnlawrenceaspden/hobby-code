;; It turns out the clojure is fast.

;; Let's try to solve this problem

;; Numerical solution of dy/dt = f (t,y)

;; Where f = t - y, and y(0) = 0

;; The exact solution is of course, y = e^(-t)+t-1
;; (because dy/dt = -e^(-t)+1 = t-y)
;; So y(1) = e^-1 = (Math/exp -1) = 0.36787944117144233

;; To solve a differential equation like this, you use a runge-kutta method
;; and the simplest runge-kutta is euler's method:

;; Take h the step size to be 0.01, y(0)=0, t(0)=0

;; And iterate h(n+1)=h(n), t(n+1)=t(n)+h, y(n+1)=y(n)+f(y(n), t(n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For this program, I care how fast the iteration is, so, since what
;; get measured gets improved:

;; Tell the compiler to warn me about any places that it needs to use
;; reflection, which is intrinsically slow:
(set! *warn-on-reflection* true)

;; And let's get our cpu speed so that we can measure speed in terms of
;; iterations per clock cycle.
;; The only way I can find to do this in clojure is to read the file
;; /proc/cpuinfo, and for some reason slurp("/proc/cpuinfo") doesn't work,
;; So I have to do it the hard way:
(require 'clojure.contrib.string)

(defn proc-cpuspeed []
  "find out the cpu speed in GHz by reading /proc/cpuinfo"
  (let [procspeed  (/ (read-string
                       (clojure.contrib.string/replace-by
                        #"cpu MHz\t\t: " (constantly "")
                        (first (clojure.contrib.string/grep
                                #"cpu MHz.*"
                                (line-seq
                                 (java.io.BufferedReader.
                                  (java.io.FileReader.
                                   (java.io.File. "/proc/cpuinfo"))))))))
                      1000.0)]
    (print "cpuspeed from proc" procspeed "GHz")
    procspeed))

(proc-cpuspeed);; 2.399 GHz on desktop, 1.6 on laptop, but there are two cores there.
;; often when laptop is idle, the cpu will cut down to 0.8, which will screw things up fairly royally!

;; There has got to be a better way to do this, but Lord, the roman hyacinths are blooming in bowls and 

(def *cpuspeed*
  (let [speed (proc-cpuspeed)]
    (println "speed measured to be " speed)
    (if (or (= speed 0.8) (= speed 1.6))
      (do 
         ;; fix the damned thing to 1.6 for laptop
        (println "fixing speed to 1.6GHz on the assumption that I'm on netbook")
        1.6)
      (do
        (println "fixing speed to" speed "GHz")
        speed))))

;; Forgive me.

;; Now we can define a microbenchmarking macro which takes an expression
;; and the number of iterations that its calculation represents, and
;; tell us how many cpu cycles went into every iteration.

;; Do computers still work like this?
;; When I were a lad everything were different...
(defmacro cyclesperit [expr its]
  `(let [start# (. System (nanoTime))
         ret# ( ~@expr ~its )
         finish# (. System (nanoTime))]
     (int (/ (* *cpuspeed* (- finish# start#)) ~its))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enough. Here is the program in which I am interested:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Runge-Kutta solver as originally written. This is how I'd do it if
;; I wasn't interested in the intermediate results, just the final value of y

(defn f [t y] (- t y))

(defn solveit [t0 y0 h its]
  (if (> its 0) 
    (let [t1 (+ t0 h)
          y1 (+ y0 (* h (f t0 y0)))]
      (recur t1 y1 h (dec its)))
    [t0 y0 h its]))

;; Let's see how it goes: remember y(1) = e^-1 0.36787944117144233
(def results (map #(second (solveit 0.0 0.0 (/ 1.0 %) %)) '(1 10 100 1000 10000 100000)))
;;(0.0 0.34867844010000004 0.3660323412732297 0.36769542477096434 0.367861046432899 0.3678776017662642)
(map #(- (Math/exp -1) %) results)
;;(0.36787944117144233 0.019201001071442292 0.001847099898212634
;; 1.8401640047799317E-4 1.8394738543314748E-5 1.8394051781167597E-6)

;; Ten times more iterations leads to a ten times better result, which we'd
;; expect from theory.


;; And here's an expression that times it over 100000 iterations

(cyclesperit (solveit 0.0 1.0 0.01) 100000)
2374
2070
1816
1721
1689
1760

;; Interestingly, the time takes a few goes to settle down. Presumably something
;; somewhere is getting the hang of running this expression.

;; But of course, 1700 cpu cycles to do a loop seems a bit poor!

;; On my desktop, the results are even worse
;; desktop
3167
2704
3005
;; laptop
1927
1762
1765
;; The laptop seems to be faster per cycle. cpu usage is (100,0) in htop while
;; the program is running, so it's not that the laptop is somehow taking
;; advantage of its two cores.

;; So how do we make it faster?
(cyclesperit (solveit 0.0 1.0 0.01) 100000)

;; There's a fairly significant speed up to be had from
;; killing off function calls,
;; I think this is because primitives don't make it through function boundaries
;; They need to be boxed and unboxed.

;; We'll inline f and create an internal target for recur, using casts to doubles
;; to make sure that inside the loop/recur, only java primitives are seen.

(defn solveit [t0 y0 h its]
  (loop [t0 (double t0) y0 (double y0) h (double h) its (int its)]
    (if (> its 0) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

;; Much better, but still rather high.
;;desktop
570
580
564
;;laptop
384
384
386
(cyclesperit (solveit 0.0 1.0 0.01) 10000000)

;; At this point, we run jvisualvm, and excellent piece of software
;; that can be installed with
;; # sudo apt-get visualvm
;; the profiler suggest integer casts are done every time round the loop,
;; and are very expensive

;; Another factor of ten comes from the (> its 0) call.  It seems that even
;; though clojure knows that its is an int, it can't figure out that 0 is??
(defn solveit [t0 y0 h its]
  (loop [t0  (double t0) y0  (double y0) h (double h) its (int its)]
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
;; Were now running round the loop 100 million times, and the startup effect
;; seems to have gone away.

;; However, it seems that we can find another factor of two here by making an
;; explicit variable for zero.  The Lord alone knows what is going on here:
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

;; The profiler is now telling me that there are
;; 2 adds, 1 gt, 1 minus, 1 dec and 1 multiply in every loop,
;; which is what I'd expect if I was writing assembler,
;; but I'm suspicious that it can tell! Presumably there's still some dispatching
;; going on?

;; With 4 floating point, 1 gt, 1 dec, and 1 conditional branch I'd imagine that
;; 7 cycles/loop would be as good as it gets without being clever.

;; So it appears that there's only a factor of 3 on laptop between this loop
;; as written, and what I'd expect from a C program!

(time (solveit 0.0 1.0 0.01 100000000))
;;laptop
"Elapsed time: 1215.044133 msecs"
[1000000.0007792843 999999.000779284 0.01 0]
;;desktop
"Elapsed time: 1130.129868 msecs"
[1000000.0007792843 999999.000779284 0.01 0]

;; 1.1 seconds to do 100 000 000 iterations on my desktop.

;; 1.2 seconds on my laptop, which has about half the clock speed, and which
;; seems to take 19 cycles for every loop

;; I'm pretty happy with that, especially given that the loop is still readable!
;; It's only slightly more complicated than the original.

;; Does anyone have any ideas how to bum a few more cycles out of the loop?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Premature optimization is the root of all evil
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Of course, it's a major sin to optimize like this







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy sequence method.  I'm assuming there's no way to optimize this because
;; it has to involve function call?
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

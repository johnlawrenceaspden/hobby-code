;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clojure is Fast!
;;
;; Paul Graham said that Common Lisp was two languages, one for writing programs
;; fast, and one for writing fast programs.  I've never tried to find Clojure's
;; fast bits before, but I thought I'd give it a try.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We need a very simple procedure to experiment on.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's try to Integrate a Differential Equation.

;; Don't be scared. That means that we've got a number that needs to change over
;; time, and a function that tells us how much it needs to change by.

;; You've got a variable y, say it's 0 (feet). We start at time 0 (seconds).

;; We calculate f(0,0), lets say that's 0. (feet/second)

;; Then y has to change by 0 feet per second. So after a tenth of a second we
;; calculate that t should be 0.1 seconds, y should still be about 0 feet, and
;; that lets us work out roughly what f is now.

;; Say f is 0.1 : then y needs to change by 0.1 feet/second. So after another
;; tenth of a second, t is 0.2, y is roughly 0.01, and we can work out f again.

;; And that's how you find an approximate numerical solution to the differential
;; equation:

;; dy/dt = f(t,y) where f(t, y) = t-y and y=0 when t=0.

;; using a step size of one-tenth of a second.

;; This challenging procedure is known as Euler's method,
;; or sometimes as first-order Runge-Kutta.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Test Case

;; As it happens, we can work out by devious mathematical trickery that the
;; exact solution (which is what happens if you make the steps so small that you
;; can't tell they're steps any more, and everything is nice and smooth) to this
;; equation is y=e^(-t)+t-1

;; So if we write our program correctly then when t is 1,
;; y should be close to (Math/exp -1) = 0.36787944117144233
;; And it should get closer if we make our steps smaller.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For this program, I care how fast the iteration is, so, since what
;; get measured gets improved:

;; I'm going to tell the compiler to warn me about any places that it needs to
;; use reflection, which is intrinsically slow, although it turns out that there
;; aren't any, so you can forget about this line:
(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What is the clock speed of my CPU?

;; Let's get our cpu speed so that we can measure speed in terms of
;; iterations per clock cycle.

;; Do computers still work like this?  When I were a lad everything were
;; different... I imagine that there's all sorts of stuff going on under various
;; hoods nowadays.

;; The only way I can find to get the cpu speed in Clojure is to read the file
;; /proc/cpuinfo, and for some reason slurp("/proc/cpuinfo") doesn't work,
;; so I have to do it the hard way:
(require 'clojure.contrib.string)

(defn proc-cpuspeed []
  "Find out the cpu speed in GHz by reading /proc/cpuinfo.
 Almost certainly doesn't work in Windows. Macs?"
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
    (println "cpuspeed from proc" procspeed "GHz")
    procspeed))

(proc-cpuspeed);; 2.399 GHz on my desktop, 1.6 on my laptop, but there are two cores
;; in the laptop.  Also, often, when laptop is idle, the cpu will cut down to 0.8, which
;; will screw things up fairly royally!

;; There has got to be a better way to do this, but Lord, the Roman hyacinths are blooming in bowls and 

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now we can define a microbenchmarking macro which takes an expression
;; and the number of iterations that its calculation represents, and
;; tell us how many cpu cycles went into every iteration.

(defmacro cyclesperit [expr its]
  `(let [start# (. System (nanoTime))
         ret# ( ~@expr (/ 1.0 ~its) ~its )
         finish# (. System (nanoTime))]
     (int (/ (* *cpuspeed* (- finish# start#)) ~its))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enough. Here is the program in which I am interested:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Runge-Kutta solver as originally written. This is how I'd do it if
;; I wasn't interested in the intermediate results, just the final value of y.

(defn f [t y] (- t y))

(defn solveit [t0 y0 h its]
  (if (> its 0) 
    (let [t1 (+ t0 h)
          y1 (+ y0 (* h (f t0 y0)))]
      (recur t1 y1 h (dec its)))
    [t0 y0 h its]))

;; Let's see how it goes: remember y(1) should approximate e^-1 0.36787944117144233
(def results (map #(second (solveit 0.0 0.0 (/ 1.0 %) %)) '(1 10 100 1000 10000 100000)))
;;(0.0 0.34867844010000004 0.3660323412732297 0.36769542477096434 0.367861046432899 0.3678776017662642)
(map #(- (Math/exp -1) %) results)
;;(0.36787944117144233 0.019201001071442292 0.001847099898212634
;; 1.8401640047799317E-4 1.8394738543314748E-5 1.8394051781167597E-6)

;; Ten times more iterations leads to a ten times better result, which we'd
;; expect from theory.

;; And here's an expression that times the loop over 100000 iterations

(cyclesperit (solveit 0.0 1.0) 1000000)

;; laptop sample timings
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
2600
2862
2576
;; vs laptop
1927
1762
1765
;; The laptop seems to be faster per cycle. cpu usage is (100,0) in htop while
;; the program is running, so it's not that the laptop is somehow taking
;; advantage of its two cores.

;; So how do we make it faster?

;; There's a fairly significant speed-up to be had from killing off function
;; calls. I think this is because primitives don't make it through function
;; boundaries They need to be boxed and unboxed.

;; There is something a bit odd about a functional language where function calls
;; are inefficient, but I understand that great men are working on the problem.

;; In the meantime, we'll inline f by hand and create an internal target for
;; recur, using casts for the initial values to make sure that inside
;; the loop/recur, only java primitives are seen.

(defn solveit [t0 y0 h its]
  (loop [t0 (double t0) y0 (double y0) h (double h) its (int its)]
    (if (> its 0) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

(cyclesperit (solveit 0.0 1.0) 10000000)
;; Much better, but still rather high.
;;desktop
488
506
486
;;laptop
384
384
386


;; At this point, we bring in the excellent jvisualvm, an excellent piece of
;; software that can be installed with

;; # sudo apt-get visualvm

;; Just run it and everything will be obvious.

;; When using jvisualvm, you should be careful to use the most stripped down
;; clojure image possible.  I routinely require all of contrib on startup, and
;; this means that the poor profiler has to instrument something like 10000
;; classes.  This takes ages. If you start with a clean image of just clojure
;; itself (it's ok to have everything on the class path, just don't load it),
;; then it's only about 1000 classes, and everything happens 10 times faster.
;; You still need to wait about 10 seconds while turning profiling on or off.

;; Attach jvisualvm and then run

(cyclesperit (solveit 0.0 1.0) 1000000)

;; The profiling slows everything down to treacle, even the REPL, so remember to
;; de-attach it before trying to do anything that might take noticeable time.


;; I'm using EMACS to write and execute this code, so the relevant procedure
;; calls can be found by taking a profiler snapshot and looking under the SWANK
;; Worker Thread.

;; There are four innocent looking calls to add, minus, and multi, all with
;; signature (double, double).  There's one to dec(int). But there's one to
;; gt(int, Object). That only takes 20% of the time, but under it there's a
;; whole tree of other calls. 


;; The profiler suggest that function overload resolutions are being done every
;; time round the loop. Weirdly, it suggests that they're not very expensive compared to add(double,double).
;; I am suspicious, so I'm going to try changing (> its 0) to (> its (int 0))

(defn solveit [t0 y0 h its]
  (loop [t0  (double t0) y0  (double y0) h (double h) its (int its)]
    (if (> its (int 0)) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

;; remember to detach the profiler! If you don't you'll get cycle counts in the 100000s

(cyclesperit (solveit 0.0 1.0) 1000000)
;; desktop
59
58
55
;; laptop
50
50
49

;; Wow! That's made a vast difference.
;; Apparently the literal 0 was being treated as a generic object. I can see why that would be slow,
;; but the profiler said that it was only 20% of the running cost.
;; It seems more likely that removing it has somehow decontaminated the other calls.
;; Maybe it's allowing the variables to stay in registers, or something?

;; At any rate, the loop is now about ten times faster, and laptop and desktop
;; are looking much more comparable in terms of cycles/iteration.

;; Let's have another look with the profiler:

(cyclesperit (solveit 0.0 1.0) 1000000)

;; Again, the profiling looks about what you'd expect, except that a method
;; called RT.intCast is being called just as often as the multiplies, minuses,
;; and decs that I'm expecting to see. The profiler claims that it's not taking
;; up much time, but let's try to get rid of it by making an explicit variable
;; for zero.  
(defn solveit [t0 y0 h its]
  (let [zero (int 0)]
    (loop [t0 (double t0) y0 (double y0) h (double h) its (int its)]
      (if (> its zero) 
        (let [t1 (+ t0 h)
              y1 (+ y0 (* h (- t0 y0)))]
          (recur t1 y1 h (dec its)))
      [t0 y0 h its]))))

;; Remove profiler and re-time:
(cyclesperit (solveit 0.0  1.0) 100000000)

;;desktop
24
24
24

;;laptop

19
19
20

;; Doing the (int 0) outside the loop again seems to have doubled the speed of
;; the loop.

;; The profiler is now telling me that there are: 2 adds(double,double), 1
;; gt(int,int), 1 minus(double, double), 1 dec(int) and 1 multiply(double,
;; double) in every loop, which is what I'd expect if I was writing C or Java to
;; do this, but I'm suspicious that it can tell! Presumably there's still some
;; dispatching going on?

;; With 4 floating point, 1 gt, 1 dec, and 1 conditional branch I'd imagine that
;; 7 cycles/loop would be as fast as this loop could be made to run without being clever.

;; So it appears that there's only around a factor of 3 between this loop
;; as written, and what I'd expect from a C, Java or assembler program. 

;; In absolute terms:

"Elapsed time: 1027.354276 msecs"
[1000000.0007792843 999999.000779284 0.01 0]

"Elapsed time: 1025.422183 msecs"
[0.09999999995254745 0.9096748359793724 1.0E-9 0]
"Elapsed time: 1026.64446 msecs"
[1.0000000022898672 0.7357588790870762 1.0E-8 0]
(time (solveit 0.0 1.0 (/ 1.0 100000000) 100000000))
;;laptop
"Elapsed time: 1215.044133 msecs"
[1000000.0007792843 999999.000779284 0.01 0]
;;desktop
"Elapsed time: 1130.129868 msecs"
[1000000.0007792843 999999.000779284 0.01 0]

;; 1.1 seconds to do 100 000 000 iterations on my desktop, at about 24 cycles/loop

;; 1.2 seconds on my laptop, which has about half the clock speed, and which
;; seems to take 19 cycles for every loop

;; I'm pretty happy with that, especially given that the loop is still readable!
;; It's only slightly more complicated than the original.

;; Does anyone have any ideas how to bum a few more cycles out of the loop?


;; So we can make it go pretty fast. Does it still work?

;; Remember y(1) should approximate e^-1 0.36787944117144233, and our vast speed up means that it's now not unreasonable to throw 1 000 000 000 iterations
;; at the problem.
(def results (map #(second (solveit 0.0 0.0 (/ 1.0 %) %)) '(1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)))
;; (0.0 0.34867844010000004 0.3660323412732297 0.36769542477096434 0.367861046432899 0.3678776017662642 0.3678792572317447 0.3678794227282174 0.3678794397549051 0.3678794410553999)

(map #(- (Math/exp -1) %) results)
;;(0.36787944117144233 0.019201001071442292 0.001847099898212634 1.8401640047799317E-4 1.8394738543314748E-5 1.8394051781167597E-6 1.8393969763996765E-7 1.8443224947262138E-8 1.4165372208552185E-9 1.1604245342411446E-10)

;; Cool! Nine Significant Figures.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Premature optimization is the root of all evil
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Of course, it's a major sin to optimize like this before you optimize your algorithm.

;; Since the problem that we're solving is smooth, we could use a higher order Runge-Kutta method instead:
;; Here's the mighty Runge Kutta order 4, usually known as 'the' Runge-Kutta method.

;; By analogy with the previous function, I'm going to define locals for 0, 2.0
;; and 6.0, which are all needed in the fourth order formula. Otherwise it's very similar.

(defn solveit [t0 y0 h its]
  (let [zero (int 0) two (double 2) six (double 6)]
    (loop [t0 (double t0) y0 (double y0) h (double h) its (int its)]
      (if (> its zero) 
        (let [h2 (/ h two)
              k1 (- t0 y0)
              k2 (- (+ t0 h2) (+ y0 (* h2 k1)))
              k3 (- (+ t0 h2) (+ y0 (* h2 k2)))
              k4 (- (+ t0 h)  (+ y0 (* h k3)))
              t1 (+ t0 h)
              y1 (+ y0 (* (/ h six)
                          (+ (+ k1 (* k2 two)) (+ (* two k3) k4))))]
          (recur t1 y1 h (dec its)))
      [t0 y0 h its]))))


(cyclesperit (solveit 0.0  1.0) 10000000)
;; desktop
177
147
145

;; This takes about five times longer per step, which looks about right for the extra complexity of the calculation,
;; so let's drop the last couple of trials because they take a while and check whether the new function works.


;; True answer: 0.36787944117144233
(def results (map #(second (solveit 0.0 0.0 (/ 1.0 %) %)) '(1 10 100 1000 10000 100000 1000000 10000000)))
;;(0.375 0.3678797744124984 0.3678794412023557 0.36787944117144566 0.3678794411714117 0.36787944117113575 0.3678794411715326 0.36787944112224436)

(map #(- (Math/exp -1) %) results)
;;(-0.007120558828557666 -3.3324105608301124E-7 -3.0913382964570246E-11 -3.3306690738754696E-15 3.064215547965432E-14 3.06588088250237E-13 -9.026113190202523E-14 4.919797902402934E-11)

;; So by being a bit smarter, we can get 15 significant figures with only 1000 iterations.

;; It looks as though using 10 times as many steps makes us 10000 times more
;; accurate now, which again is in accordance with the theory. The four zeros in
;; 10000 are why it's called 'fourth order'.

;; But actually, after 1000 iterations, the accuracy actually gets worse!

;; We're running into floating point 'noise' here.  With 10 000 000 iterations,
;; the tiny errors in the floating point calculations are adding up to dominate
;; the error in the approximation method. First order Runge-Kutta isn't good
;; enough to see this effect, although it is there.  Presumably if we'd tried
;; running the method for 100 billion iterations then we wouldn't have got any
;; more accurate. Which would have been a disappointment after hours of computing.
;; Without using an exact test case, there would have been no way to tell.



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

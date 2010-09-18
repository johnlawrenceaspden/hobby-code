;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clojure is Fast!

;; Paul Graham said that Common Lisp was two languages, one for writing programs
;; fast, and one for writing fast programs.

;; I've never tried to find Clojure's fast bits before, but I thought I'd give
;; it a try, using a simple example of a numerical algorithm that C and FORTRAN
;; would be very good for.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's try to integrate a differential equation.

;; Don't be scared! That means that we've got a number that needs to change over
;; time, and a function that tells us how much it needs to change by.

;; You've got a variable y, say it's 0 (feet). We start at time 0 (seconds).

;; We calculate f(0,0), lets say that's 0. (feet/second)

;; Then y has to change by 0 feet per second. So after a tenth of a second we
;; calculate that t should be 0.1 seconds, y should still be about 0 feet, and
;; that lets us work out roughly what f is now.

;; Say f is 0.1 : then y needs to change by 0.1 feet/second. So after another
;; tenth of a second, t is 0.2, y is roughly 0.01, and we can work out f again.

;; And repeat, for as many steps as you're interested in.

;; And that's how you find an approximate numerical solution to the differential
;; equation:

;; dy/dt = f(t,y) where f(t, y) = t-y and y=0 when t=0.

;; using a step size of one-tenth of a second.

;; This challenging procedure is known as Euler's Method, or sometimes as
;; first-order Runge-Kutta.

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

;; So that's the scene set. Here is the program in which I am interested:

(defn f [t y] (- t y))

(defn solveit [t0 y0 h its]
  (if (> its 0) 
    (let [t1 (+ t0 h)
          y1 (+ y0 (* h (f t0 y0)))]
      (recur t1 y1 h (dec its)))
    [t0 y0 h its]))


;; And here's an invocation: start from 0.0 at time 0.0, step size is 0.1, run for 10 iterations

(solveit 0.0 0.0 0.1 10)
;; [0.9999999999999999 0.34867844010000004 0.1 0]

;; The answer tells us that after 10 steps t is 0.999..., or 1 as it's
;; traditionally known, and y is 0.348678....  The other two numbers are the
;; step size and the remaining iteration count, now down to 0 because the
;; process has done its ten steps.

;; In the exact answer, when t is 1, y should be e^-1, or 0.36787944117144233.
;; So the answer's right to within 0.02, which is a good indicator that the
;; process works.

;; Let's have a look at the answers with different numbers of steps:

(let [steps '(1 10 100 1000 10000 100000)
      results (map #(second (solveit 0.0 0.0 (/ 1.0 %) %)) steps )
      errors  (map #(- (Math/exp -1) %) results)]
  (partition 3 (interleave steps results errors)))

;; steps result              error
((1      0.0                 0.36787944117144233)
 (10     0.34867844010000004 0.019201001071442292)
 (100    0.3660323412732297  0.001847099898212634)
 (1000   0.36769542477096434 1.8401640047799317E-4)
 (10000  0.367861046432899   1.8394738543314748E-5)
 (100000 0.3678776017662642  1.8394051781167597E-6))

;; Ten times more iterations leads to a ten times better result, which we'd
;; expect from theory. That's why it's called a first order method.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For this program, I care how fast the iteration is.
;; What gets measured gets improved:

(def *cpuspeed* 2.399) ;; My computer runs at 2.399 GHz

;; We can define a microbenchmarking macro which takes an expression
;; and the number of iterations that its calculation represents, and
;; tell us how many cpu cycles went into every iteration.

(defmacro cyclesperit [expr its]
  `(let [start# (. System (nanoTime))
         ret# ( ~@expr (/ 1.0 ~its) ~its )
         finish# (. System (nanoTime))]
     (int (/ (* *cpuspeed* (- finish# start#)) ~its))))

;; So here's an expression which times the loop over 100000 iterations.

(cyclesperit (solveit 0.0 1.0) 1000000)

;; What are we expecting? Well, if modern computers work the same way as the
;; computers I used to write assembly language for, then we can estimate thus:

;; Here's the program again:
(defn f [t y] (- t y))

(defn solveit [t0 y0 h its]
  (if (> its 0) 
    (let [t1 (+ t0 h)
          y1 (+ y0 (* h (f t0 y0)))]
      (recur t1 y1 h (dec its)))
    [t0 y0 h its]))

;; For every go round the loop we have to:
;; compare its with 0,
;; branch depending on the result,
;; add t0 to h,
;; call f with t0 and y0,
;; multiply h and the result,
;; add that to y0,
;; jump.

;; So if this was an assembly language program that worked the way you'd expect,
;; each loop would take 7 cycles.

;; This estimate turns out to have been a little optimistic.

;; On my desktop machine, the results of the timing expression
(cyclesperit (solveit 0.0 1.0) 1000000)
;; over four trial runs are:
2382
2290
2278
2317

;; So we're looking at a slowdown of about 300 times over what we could probably
;; achieve coding in assembler or in C with a good optimizing compiler (and of
;; course I'm assuming that floating point operations take one cycle each)

;; This is about the sort of speed that you'd expect from a dynamic language
;; without any optimization or type hinting.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So how do we make it faster?

;; There's a fairly significant speed-up to be had from killing off function
;; calls. I think this is because primitives don't make it through function
;; boundaries They need to be boxed and unboxed.

;; There is something a bit odd about a functional language where function calls
;; are inefficient, but I understand that great men are working on the problem,
;; so it will probably not be a problem for clojure 1.3

;; In the meantime however, we'll inline f by hand and we'll create an internal
;; target for recur, using casts on the initial values to make sure that inside
;; the loop/recur, only the java primitives int and double are seen:

(defn solveit-2 [t0 y0 h its]
  (loop [t0 (double t0), y0 (double y0), h (double h), its (int its)]
    (if (> its 0) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

;; Let's time that and see how it goes:
(cyclesperit (solveit-2 0.0 1.0) 10000000)
488
506
486

;; That's much better. The slowdown is now about 70 times compared with the
;; program and CPU in my head.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The first law of optimizing things is that you need a profiler to find out
;; where the slow bits are.

;; At this point, we'll bring in jvisualvm, an excellent piece of software that
;; can be installed on Ubuntu with:

;; # sudo apt-get visualvm

;; and probably with something similar on any other system where Java will run.

;; Just run it. How it works should be fairly obvious. I'm sure there are
;; docs and stuff. I haven't looked.

;; When using jvisualvm, you should be careful to use the most stripped-down
;; clojure image possible.

;; I usually 'require' all of contrib on startup, and
;; this means that the poor profiler has to instrument something like 10000
;; classes.  This takes ages.

;; If you start with a clean image (it's ok to have everything on the classpath,
;; just don't load it if you don't need it), then it's only about 1000 classes,
;; and everything happens 10 times faster.  You still need to wait about 10
;; seconds while turning profiling on or off, but that's bearable.

;; Attach jvisualvm to your clojure, and then run

(cyclesperit (solveit-2 0.0 1.0) 1000000)

;; The profiling slows everything down to treacle, even the REPL, so remember to
;; de-attach it before trying to do anything that might take noticeable time.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Results of profiling:

;; There are four innocent looking calls to add, minus, and multi, all with
;; signature (double, double).  There's one to dec(int). But there's also one to
;; gt(int, Object). That only takes 20% of the time, apparently, but under it
;; there's a whole tree of other calls. Something is getting resolved at run
;; time, which is usually bad for speed.

;; The profiler suggest that function overload resolutions are being done every
;; time round the loop. Weirdly, it suggests that they're not very expensive
;; compared to add(double,double).  I am suspicious, so I'm going to try
;; changing (> its 0) to (> its (int 0)). That should allow the compiler to work
;; out the type of the call to > at compile time, rather than every time round.

(defn solveit-3 [t0 y0 h its]
  (loop [t0 (double t0), y0 (double y0), h (double h), its (int its)]
    (if (> its (int 0)) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

;; Let's time that:
;; Remember to detach the profiler! If you don't you'll get cycle counts in the 100000s

(cyclesperit (solveit-3 0.0 1.0) 1000000)
79
79
63

;; Wow! That's made a vast difference. I don't understand why.

;; Apparently the literal 0 was being treated as a generic object. I can see why
;; that would be slow, but the profiler said that it was only 20% of the running
;; cost.  It seems more likely that removing it has somehow decontaminated the
;; other calls.  Maybe it's allowing the variables to stay in registers where
;; before they were being pushed out back onto the heap, or something?

;; I wonder if there's a way to examine the code that clojure generates for a
;; function?

;; At any rate, the loop is now about six times faster than it was.

;; Let's have another look with the profiler:
;; Attach it and run:

(cyclesperit (solveit-3 0.0 1.0) 1000000)

;; Again, the profiling looks about what you'd expect, except that a method
;; called RT.intCast is being called just as often as the multiplies, minuses,
;; and decs that I'm expecting to see. The profiler claims that it's not taking
;; up much time, but let's try to get rid of it by making an explicit local
;; variable for zero. For some reason this reminds me of ZX81 BASIC.
(defn solveit-4 [t0 y0 h its]
  (let [zero (int 0)]
    (loop [t0 (double t0) y0 (double y0) h (double h) its (int its)]
      (if (> its zero) 
        (let [t1 (+ t0 h)
              y1 (+ y0 (* h (- t0 y0)))]
          (recur t1 y1 h (dec its)))
      [t0 y0 h its]))))

;; Remove the profiler and re-time:
(cyclesperit (solveit-4 0.0  1.0) 100000000)
23
23
23


;; Doing the (int 0) outside the loop again seems to have tripled the speed of
;; the loop again.

;; The profiler is now telling me that there are: 2 adds(double,double), 1
;; gt(int,int), 1 minus(double, double), 1 dec(int) and 1 multiply(double,
;; double) in every loop, which is what I'd expect if I was writing C or Java to
;; do this, but I'm suspicious that it can tell! Presumably there's still some
;; dispatching going on? These should be single assembler instructions, and
;; invisible to a profiler working at function level.

;; With 4 floating point, 1 gt, 1 dec, and 1 conditional branch I'd imagine that
;; 7 cycles/loop would be as fast as this loop could be made to run without being clever.

;; So it appears that there's now only around a factor of 3 between this loop as
;; written, and what I'd expect from a C, Java or assembler program.

;; In absolute terms:

"Elapsed time: 1019.442664 msecs"
[1.0000000022898672 0.7357588790870762 1.0E-8 0]
(time (solveit-4 0.0 1.0 (/ 1.0 100000000) 100000000))


;; 1 second to do 100 000 000 iterations on my desktop, at about 23 cycles/loop

;; I'm pretty happy with that, especially given that the loop is still readable!
;; It's only slightly more complicated than the original. Optimizing Common Lisp
;; tends to make it look horrible.

;; Does anyone have any ideas how to squeeze a few more cycles out of the loop?

;; One more thing. We can make it go pretty fast. Does it still work?

;; Remember y(1) should approximate e^-1 0.36787944117144233, and our vast
;; speedup means that it's now not unreasonable to throw 1 000 000 000
;; iterations at the problem.

(let [steps '(1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000)
      results (map #(second (solveit-4 0.0 0.0 (/ 1.0 %) %)) steps )
      errors  (map #(- (Math/exp -1) %) results)]
  (partition 3 (interleave steps results errors)))

((1          0.0                 0.36787944117144233)
 (10         0.34867844010000004 0.019201001071442292)
 (100        0.3660323412732297  0.001847099898212634)
 (1000       0.36769542477096434 1.8401640047799317E-4)
 (10000      0.367861046432899   1.8394738543314748E-5)
 (100000     0.3678776017662642  1.8394051781167597E-6)
 (1000000    0.3678792572317447  1.8393969763996765E-7)
 (10000000   0.3678794227282174  1.8443224947262138E-8)
 (100000000  0.3678794397549051  1.4165372208552185E-9)
 (1000000000 0.3678794410553999  1.1604245342411446E-10))

;; Cool! Accuracy improves as predicted, and with 10^9 steps we get nine
;; significant figures in about ten seconds.

(time (solveit-4 0.0 1.0 (/ 1.0 1000000000) 1000000000))

;; Note:
;;
;; Just in order to keep my credibility as a numerical analyst intact, I ought
;; to point out that if I really was trying to solve a smooth ODE (instead of
;; investigating how fast I could make some simple numeric code in Clojure), I
;; wouldn't be using Euler's method. Optimize your algorithm before you optimize
;; your code.

;; No differential equations were harmed in the making of this blogpost.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conclusion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clojure is a fast language, if you write so that your intentions are clear to
;; the compiler.  Something tells me that as clojure gets older, it will be
;; getting better at working out what your intentions are.

;; It would not surprise me in the slightest if very soon, the code as originally
;; written runs as fast or faster than my speeded up version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
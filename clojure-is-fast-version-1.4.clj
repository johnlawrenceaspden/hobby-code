;; Is Clojure Still Fast ?

;; Once upon a time I wrote a blog post saying that clojure was fast.  It still is, and optimizing
;; it is now much easier than it used to be, but it doesn't seem to be *quite* as fast as it once
;; was.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I optimized a program to solve a differential equation in a
;; simple-minded way:

;; The equation was: dy/dt = f(t,y) where f(t, y) = t-y and y=0 when t=0
;; ( the exact solution is: y=e^(-t)+t-1 )

;; Here's a program to solve it using Euler's method

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

;; So if this was an assembly language program that worked the way
;; you'd expect, each loop would take 7 cycles.

;; According to /proc/cpuinfo, my netbook runs at 1.662 GHz.

(def *cpuspeed* 1.662)

;; We care about how many cycles each iteration of the solver takes:

(defmacro cyclesperit [expr its]
  `(let [start# (. System (nanoTime))
         ret# ( ~@expr (/ 1.0 ~its) ~its )
         finish# (. System (nanoTime))]
     (int (/ (* *cpuspeed* (- finish# start#)) ~its))))


;; With the program as written, this estimate turns out to have been a little optimistic.

;; The figures in the original post were on a desktop machine that I no longer have, which was more
;; powerful than my current netbook.

;; To two significant figures, the results of the timing expression
(cyclesperit (solveit 0.0 1.0) 1000000) 
;; are:

;; On my old desktop with clojure 1.2 : 2300 cycles

;; On my netbook with clojure 1.2 : 2800 cycles
;; On my netbook with clojure 1.3 : 2500 cycles
;; On my netbook with clojure 1.4 : 2400 cycles

;; So it looks like my netbook is not only slower in clock speed than my desktop was, but also in
;; terms of cycles/iteration. That's not surprising as the netbook has an Atom processor, optimized
;; for low power rather than for speed.

;; But it also looks as though clojure has been speeding up slightly, which has almost made up for that.

;; I'm also assuming that the JVM itself hasn't changed much since the original blog post.  The
;; netbook timings were all done today on the same JVM, but the desktop timings are from a while
;; back, so that might account for some differences.

;; So we're looking at a slowdown of about 300 times over what we could probably
;; achieve coding in assembler or in C with a good optimizing compiler (and of
;; course I'm assuming that floating point operations take one cycle each)

;; This is about the sort of speed that you'd expect from a dynamic language
;; without any optimization or type hinting.

;; In the original blog post I went through a number of faster versions:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In solveit-2 I explicitly typed the loop variables, inlined the function call, and made an
;; internal target for recur. That speeded things up considerably:

(defn solveit-2 [t0 y0 h its]
  (loop [t0 (double t0), y0 (double y0), h (double h), its (int its)]
    (if (> its 0) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

;; Let's time that and see how it goes:
(cyclesperit (solveit-2 0.0 1.0) 10000000) 

;; On my old desktop with clojure 1.2 : 490 cycles

;; On my netbook with clojure 1.2 : 600 cycles
;; On my netbook with clojure 1.3 : 44 cycles
;; On my netbook with clojure 1.4 : 44 cycles


;; Wow!

;; Again we see that my netbook is around 20% slower, but the performance change between clojure 1.2
;; and clojure 1.3 was incredible.

;; There's now about a factor of 6 between the clojure version and the imaginary assembler program
;; on the imaginary cpu in my head. And all I've done is to declare the types and inline the functions.

;; This is actually so impressive that I want to examine the three things in detail, to see how the
;; three things combine to cause the speedup. As I remember from clojure 1.2, you needed all three
;; changes to see any great difference. In 1.4 you can type hint separately from making an internal loop target.
;; All timings with clojure 1.4.0.

(defn f [t y] (- t y))

(defn solveit [t0 y0 h its]
  (if (> its 0) 
    (let [t1 (+ t0 h)
          y1 (+ y0 (* h (f t0 y0)))]
      (recur t1 y1 h (dec its)))
    [t0 y0 h its]))

(cyclesperit (solveit 0.0 1.0) 10000000) ; 2481

;; type hints: 

(defn ^double tf [^double t ^double y] (- t y))

(defn solveit-1-1 [^double t0 ^double y0 ^double h ^long its]
  (if (> its 0) 
    (let [t1 (+ t0 h)
          y1 (+ y0 (* h (tf t0 y0)))]
      (recur t1 y1 h (dec its)))
    [t0 y0 h its]))

(cyclesperit (solveit-1-0 0.0 1.0) 10000000) ; 289

;; Inline f
(defn solveit-1-2 [ ^double t0 ^double y0 ^double h ^long its]
  (if (> its 0) 
    (let [t1 (+ t0 h)
          y1 (+ y0 (* h (- t0 y0)))]
      (recur t1 y1 h (dec its)))
    [t0 y0 h its]))

(cyclesperit (solveit-1-2 0.0 1.0) 10000000) ; 44

;; Internal loop target
(defn solveit-1-3 [ ^double t0 ^double y0 ^double h ^long its]
  (loop [t0 t0, y0 y0, h h, its its]
    (if (> its 0) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

(cyclesperit (solveit-1-3 0.0 1.0) 10000000) ; 44

;; Original version
(defn solveit-2 [t0 y0 h its]
  (loop [t0 (double t0), y0 (double y0), h (double h), its (int its)]
    (if (> its 0) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

;; Let's time that and see how it goes:
(cyclesperit (solveit-2 0.0 1.0) 10000000) ; 44


;; This is pretty awesome. Type hints alone are giving us a factor of eight speedup, and inlining
;; the function then gives us another factor of 6. The internal loop target, which was originally a
;; bit of a hack to allow clojure to deduce types doesn't make any difference to the hinted version,
;; but in fact that still works, and gives the compiler enough information to get the same speed.

;; So far I really could not be more impressed.

;; In the original post, I then used a profiler to find out where the loop was running slow, and 
;; did some strange things to make it fast:

(defn solveit-3 [t0 y0 h its]
  (loop [t0 (double t0), y0 (double y0), h (double h), its (int its)]
    (if (> its (int 0)) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

(cyclesperit (solveit-3 0.0 1.0) 10000000) 

;; On my old desktop with clojure 1.2 : 70 cycles

;; On my netbook with clojure 1.2 : 90 cycles
;; On my netbook with clojure 1.3 : 100 cycles
;; On my netbook with clojure 1.4 : 100 cycles

;; Originally, that (int 0) instead of 0 in the comparison made all the difference, causing a huge
;; and unexpected speedup.

;; The odd thing here is that the same program actually runs slightly slower in clojure 1.3/1.4 than
;; it does in clojure 1.2. Not much, and they're all considerably slower than clojure 1.4's best
;; shot so far.

;; What I think is happening here is that that int cast is taking up unnecessary time in the later clojures,
;; whereas in 1.2 it was the final piece of the puzzle as far as the compiler was concerned.

;; To test this:
(defn solveit-3-a [ t0 y0 h its]
  (loop [t0 (double t0), y0 (double y0), h (double h), its (long its)]
    (if (> its (long 0 )) 
      (let [t1 (+ t0 h)
            y1 (+ y0 (* h (- t0 y0)))]
        (recur t1 y1 h (dec its)))
      [t0 y0 h its])))

(cyclesperit (solveit-3-a 0.0 1.0) 10000000) ; 44

;; solveit-3-a runs at the same 44 cycles/iteration as we've been seeing before. I get the
;; impression that modern clojure prefers longs to ints.


;; The final craziness was to take the (int 0) out of the loop entirely:

(defn solveit-4 [t0 y0 h its]
  (let [zero (int 0)]
    (loop [t0 (double t0) y0 (double y0) h (double h) its (int its)]
      (if (> its zero) 
        (let [t1 (+ t0 h)
              y1 (+ y0 (* h (- t0 y0)))]
          (recur t1 y1 h (dec its)))
      [t0 y0 h its]))))

(cyclesperit (solveit-4 0.0  1.0) 100000000)

;; And this is where it gets weird:

;; On my old desktop with clojure 1.2 : 23 cycles

;; On my netbook with clojure 1.2 : 32 cycles
;; On my netbook with clojure 1.3 : 45 cycles
;; On my netbook with clojure 1.4 : 45 cycles

;; Clojure 1.3/1.4 is now running at its usual (fast) speed, despite the ints rather than longs in solveit-4

;; But Clojure 1.2 on this program is able to run about 33% faster than the modern versions.

;; So it actually looks as though whatever awesomeness has caused the phenomenal speedup between
;; clojure 1.2 and 1.3 has actually slightly slowed down the optimized version.

;; I'm imagining that this is something to do with using longs rather than ints. 

;; But I haven't done any kind of close profiling to see if I can make 1.4 run any faster than 44 cycles/loop.

;; Someone in a comment on my earlier post said that they were seeing 8 cycles/second on their
;; Macintosh, so it might just be that there's something weird about the Atom processor.

;; At any rate, it looks as though optimizing numeric code in Clojure is now dead easy. You just
;; tell it the types, inline function calls, and that's as good as it gets.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For those in real need of cycles and willing to take risks to save them, there's:

(set! *unchecked-math* true)

;; Which I don't think has an equivalent in 1.2, which cuts a few cycles off:

(defn solveit-4 [t0 y0 h its]
  (let [zero (long 0)]
    (loop [t0 (double t0) y0 (double y0) h (double h) its (long its)]
      (if (> its zero) 
        (let [t1 (+ t0 h)
              y1 (+ y0 (* h (- t0 y0)))]
          (recur t1 y1 h (dec its)))
      [t0 y0 h its]))))

(cyclesperit (solveit-4 0.0  1.0) 100000000) ; 37

;; I do wonder what is going on here. I'm not sure what checking is here to be turned off.

;; Incidentally, the following two java programs also produce the numbers that are seen above:

;; public class Euler{
;;     public static void main (String[] args){
;;         double cpuspeed = 1.662;
;;         int its = 10000000;
;;         double t=0;
;;         double y=0;
;;         double h=1.0/its;

;;         Long start = System.nanoTime();
;;         for(int i = 0; i < its; i++){
;;             y = y+h*(t-y);
;;             t = t+h;
;;         }
;;         Long finish = System.nanoTime();

;;         System.out.println("y=" + y + " t=" +t); 
;;         System.out.println("cycles/iteration: " +  ((int) ((cpuspeed * (finish - start)) / its)));
;;     }
;; }


;; y=0.3678794227282174 t=0.99999999975017
;; cycles/iteration: 32


;; public class Euler{
;;     public static void main (String[] args){
;;         double cpuspeed = 1.662;
;;         long its = 10000000;
;;         double t=0;
;;         double y=0;
;;         double h=1.0/its;

;;         long start = System.nanoTime();
;;         for(long i = 0; i < its; i++){
;;             y = y+h*(t-y);
;;             t = t+h;
;;         }
;;         long finish = System.nanoTime();

;;         System.out.println("y=" + y + " t=" +t); 
;;         System.out.println("cycles/iteration: " +  ((int) ((cpuspeed * (finish - start)) / its)));
;;     }
;; }

;; y=0.3678794227282174 t=0.99999999975017
;; cycles/iteration: 37

;; I'm worried that my favourite language seems to be becoming difficult to
;; understand.

;; Here's a favourite program of mine, in fact I remember it being one of the
;; reasons that I first liked clojure.

;; Everyone loves Fibonacci numbers, and the idea of turning a tree recursion
;; into a simple linear calculation is cool.  We can watch it happening by
;; tracing function calls.

;; Obviously nobody actually cares about memoized tree recursions on the
;; fibonacci numbers in practice. I just picked this as an example where I knew
;; what was going on, because I'd had to delve into it as a model problem in
;; order to figure out what was going on in other more complicated situations
;; where things didn't seem to be working as they should.

;; Here's how it that works in various versions of clojure.

;; Clojure 1.1

(clojure-version) ; "1.1.0"

;; Here's the classic exponentially bad way to calculate the fibonacci numbers

(defn fib1[n]
  (if (< n 2) n
      (+ (fib1 (dec n)) (fib1 (dec (dec n)))))) ; #'user/fib1

(time (fib1 30)) ; "Elapsed time: 1383.510921 msecs"
832040

;; Using dotrace, we can see why it takes so long

(require 'clojure.contrib.trace) ; nil

(clojure.contrib.trace/dotrace (fib1) (fib1 5)) ; 
;; TRACE t2452: (fib1 5)
;; TRACE t2453: |    (fib1 4)
;; TRACE t2454: |    |    (fib1 3)
;; TRACE t2455: |    |    |    (fib1 2)
;; TRACE t2456: |    |    |    |    (fib1 1)
;; TRACE t2456: |    |    |    |    => 1
;; TRACE t2457: |    |    |    |    (fib1 0)
;; TRACE t2457: |    |    |    |    => 0
;; TRACE t2455: |    |    |    => 1
;; TRACE t2458: |    |    |    (fib1 1)
;; TRACE t2458: |    |    |    => 1
;; TRACE t2454: |    |    => 2
;; TRACE t2459: |    |    (fib1 2)
;; TRACE t2460: |    |    |    (fib1 1)
;; TRACE t2460: |    |    |    => 1
;; TRACE t2461: |    |    |    (fib1 0)
;; TRACE t2461: |    |    |    => 0
;; TRACE t2459: |    |    => 1
;; TRACE t2453: |    => 3
;; TRACE t2462: |    (fib1 3)
;; TRACE t2463: |    |    (fib1 2)
;; TRACE t2464: |    |    |    (fib1 1)
;; TRACE t2464: |    |    |    => 1
;; TRACE t2465: |    |    |    (fib1 0)
;; TRACE t2465: |    |    |    => 0
;; TRACE t2463: |    |    => 1
;; TRACE t2466: |    |    (fib1 1)
;; TRACE t2466: |    |    => 1
;; TRACE t2462: |    => 2
;; TRACE t2452: => 5
;; 5

;; Luckily, there's built in memoization:

(def fib1 (memoize fib1)) ; #'user/fib1

;; Trace shows us the improved behaviour

(clojure.contrib.trace/dotrace (fib1) (fib1 5)) ; 
;; TRACE t2396: (fib1 5)
;; TRACE t2397: |    (fib1 4)
;; TRACE t2398: |    |    (fib1 3)
;; TRACE t2398: |    |    => 2
;; TRACE t2399: |    |    (fib1 2)
;; TRACE t2399: |    |    => 1
;; TRACE t2397: |    => 3
;; TRACE t2400: |    (fib1 3)
;; TRACE t2400: |    => 2
;; TRACE t2396: => 5
;; 5

;; And now the algorithm is 

(time (fib1 30)) ; "Elapsed time: 0.092197 msecs"
832040

(time (fib1 100)) ; "Elapsed time: 17.798988 msecs"
354224848179261915075

(time (fib1 300)) ; Stack Overflow

;; (Even in 1.1 we had to live with the JVM being pants. I mean really, my call
;; stack can't be more than 300 deep? The last time that was true I was writing
;; C on an 8-bit microcontroller. I was hoping this would somehow get fixed.)

;; This is a better way to calculate the fibs:

(defn fib-it [n]
  (loop [a 0 b 1 count 0]
               (if (< count n)
                   (recur b (+ a b) (inc count))
                   a))) ; #'user/fib-it

(time (fib-it 30))
;; "Elapsed time: 0.122168 msecs"
;; 832040

;; And it gets us round the jvm-is-pants problem.

(time (fib-it 300))
;; "Elapsed time: 7.483336 msecs"
;; 222232244629420445529739893461909967206666939096499764990979600

;; If we worry about speed we can put type hints in:

(defn fib-it-opt [n]
  (let [cn (int n)]
    (loop [a (int 0) b (int 1) count (int 0)]
      (if (< count cn)
        (recur b (+ a b) (inc count))
        a)))) ; #'user/fib-it-opt

;; And it gets a little faster (I haven't gone to much trouble to optimize this
;; here. I bet it can get much quicker with the aid of a profiler):

(time (fib-it-opt 30))
;; "Elapsed time: 0.083333 msecs"
;; 832040

(time (fib-it-opt 300)) ; integer overflow

;; But only at the cost of integer overflow. Fair enough.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clojure 1.2

;; In clojure 1.2, both trace and memoize are broken, but things seem a smidge faster:

(clojure-version) ; "1.2.0"

(defn fib1[n]
  (if (< n 2) n
      (+ (fib1 (dec n)) (fib1 (dec (dec n)))))) ; #'user/fib1

(time (fib1 30)) ; "Elapsed time: 1250.562385 msecs"
;; 832040

(require 'clojure.contrib.trace) ; nil

(clojure.contrib.trace/dotrace (fib1) (fib1 5))
;; TRACE t10735: (fib1 5)
;; TRACE t10735: => 5
;; 5

;; We try tracing, but we don't see the recursive calls


(def fib1 (memoize fib1)) ; #'user/fib1

(clojure.contrib.trace/dotrace (fib1) (fib1 5))
;; TRACE t10739: (fib1 5)
;; TRACE t10739: => 5
;; 5

;; And in fact, they haven't helped.

(time (fib1 30))
;; "Elapsed time: 1211.308387 msecs"
;; 832040

;; Both problems are caused by linking the recursive calls statically, so they don't go through the variable any more.

;; If we want to see the original behaviour, then we have to call through the var explicitly:

(defn fib2[n]
  (if (< n 2) n
      (+ (#'fib2 (dec n)) (#'fib2 (dec (dec n)))))) ; #'user/fib2

(time (fib2 30)) ; 
;; "Elapsed time: 1270.465348 msecs"
;; 832040

;; Explicitly linking dynamically doesn't seem to cost much in the way of speed.
;; And now, both trace and memoize work fine.

(require 'clojure.contrib.trace) ; nil

(clojure.contrib.trace/dotrace (fib2) (fib2 5)) ; 
;; TRACE t10782: (fib2 5)
;; TRACE t10783: |    (fib2 4)
;; TRACE t10784: |    |    (fib2 3)
;; TRACE t10785: |    |    |    (fib2 2)
;; TRACE t10786: |    |    |    |    (fib2 1)
;; TRACE t10786: |    |    |    |    => 1
;; TRACE t10787: |    |    |    |    (fib2 0)
;; TRACE t10787: |    |    |    |    => 0
;; TRACE t10785: |    |    |    => 1
;; TRACE t10788: |    |    |    (fib2 1)
;; TRACE t10788: |    |    |    => 1
;; TRACE t10784: |    |    => 2
;; TRACE t10789: |    |    (fib2 2)
;; TRACE t10790: |    |    |    (fib2 1)
;; TRACE t10790: |    |    |    => 1
;; TRACE t10791: |    |    |    (fib2 0)
;; TRACE t10791: |    |    |    => 0
;; TRACE t10789: |    |    => 1
;; TRACE t10783: |    => 3
;; TRACE t10792: |    (fib2 3)
;; TRACE t10793: |    |    (fib2 2)
;; TRACE t10794: |    |    |    (fib2 1)
;; TRACE t10794: |    |    |    => 1
;; TRACE t10795: |    |    |    (fib2 0)
;; TRACE t10795: |    |    |    => 0
;; TRACE t10793: |    |    => 1
;; TRACE t10796: |    |    (fib2 1)
;; TRACE t10796: |    |    => 1
;; TRACE t10792: |    => 2
;; TRACE t10782: => 5
;; 5


(def fib2 (memoize fib2)) ; #'user/fib2

(clojure.contrib.trace/dotrace (fib2) (fib2 5)) ; 
;; TRACE t10808: (fib2 5)
;; TRACE t10809: |    (fib2 4)
;; TRACE t10810: |    |    (fib2 3)
;; TRACE t10811: |    |    |    (fib2 2)
;; TRACE t10812: |    |    |    |    (fib2 1)
;; TRACE t10812: |    |    |    |    => 1
;; TRACE t10813: |    |    |    |    (fib2 0)
;; TRACE t10813: |    |    |    |    => 0
;; TRACE t10811: |    |    |    => 1
;; TRACE t10814: |    |    |    (fib2 1)
;; TRACE t10814: |    |    |    => 1
;; TRACE t10810: |    |    => 2
;; TRACE t10815: |    |    (fib2 2)
;; TRACE t10815: |    |    => 1
;; TRACE t10809: |    => 3
;; TRACE t10816: |    (fib2 3)
;; TRACE t10816: |    => 2
;; TRACE t10808: => 5
;; 5


(time (fib2 30)) ; 
;; "Elapsed time: 0.664669 msecs"
;; 832040

;; This seems to have got slower. 

(time (fib2 100)) ; "Elapsed time: 6.988696 msecs"
354224848179261915075

;; But maybe not.

(time (fib2 300)) ; And this still breaks the stack.

(defn fib-it [n]
  (loop [a 0 b 1 count 0]
               (if (< count n)
                   (recur b (+ a b) (inc count))
                   a))) ; #'user/fib-it

;; The loop/recur version is about the same speed:
(time (fib-it 30))
;; "Elapsed time: 0.126682 msecs"
;; 832040

;; But here it looks vastly faster
(time (fib-it 300)) ; "Elapsed time: 0.328122 msecs"
;; 222232244629420445529739893461909967206666939096499764990979600

(defn fib-it-opt [n]
  (let [cn (int n)]
    (loop [a (int 0) b (int 1) count (int 0)]
      (if (< count cn)
        (recur b (+ a b) (inc count))
        a)))) ; #'user/fib-it-opt

;; That version looks about the same:

(time (fib-it-opt 30)) ; "Elapsed time: 0.110864 msecs"
;; "Elapsed time: 0.083333 msecs"
;; 832040

;; And still overflows

(time (fib-it-opt 300)) ; ; integer overflow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clojure 1.3-alpha-4

(clojure-version) ; "1.3.0-alpha4"

(defn fib1[n]
  (if (< n 2) n
      (+ (fib1 (dec n)) (fib1 (dec (dec n)))))) ; #'user/fib1

(time (fib1 30))
;; "Elapsed time: 1174.800486 msecs"
;; 832040

(require 'clojure.contrib.trace) ; nil

;; And the trace function is more broken:
(clojure.contrib.trace/dotrace (fib1) (fib1 5)) ; Evaluation Aborted

;; We can catch and print the exception:
(try
  (clojure.contrib.trace/dotrace (fib1) (fib1 5))
  (catch Exception e e)) ; #<IllegalStateException java.lang.IllegalStateException: Can't dynamically bind non-dynamic var: user/fib1>

;; Memoization seems to work
(def fib1 (memoize fib1)) ; #'user/fib1

;; But trace can't tell us what's really happening:
(clojure.contrib.trace/dotrace (fib1) (fib1 5)) ; ; Evaluation Aborted

;; Let's define an exception-catching macro since it seems to be happening a lot.
(defmacro report [& body]
  `(try
     ~@body
     (catch Throwable e# e#)))

(report (clojure.contrib.trace/dotrace (fib1) (fib1 5)))
;; #<IllegalStateException java.lang.IllegalStateException: Can't dynamically bind non-dynamic var: user/fib1>

;; However, just from the timings

(time (fib1 30)) ; "Elapsed time: 0.800641 msecs"
;; 832040

;; We can tell that the memoization has worked. Although it still seems slower than 1.1

;; We try the dynamic variable trick again

(defn fib2[n]
  (if (< n 2) n
      (+ (#'fib2 (dec n)) (#'fib2 (dec (dec n)))))) ; #'user/fib2

;; It's got noticeably slower now.

(time (fib2 30))
;; "Elapsed time: 1471.117141 msecs"
;; 832040

;; Trace is still broken:
(report (clojure.contrib.trace/dotrace (fib2) (fib2 5)))  
;; #<IllegalStateException java.lang.IllegalStateException: Can't dynamically bind non-dynamic var: user/fib2>


(def fib2 (memoize fib2)) ; #'user/fib2

(report (clojure.contrib.trace/dotrace (fib2) (fib2 5))) ; #<IllegalStateException java.lang.IllegalStateException: Can't dynamically bind non-dynamic var: user/fib2>

(time (fib2 30))
;; "Elapsed time: 1.772153 msecs"
;; 832040


;; We can label a function explicitly as :dynamic.

(defn ^:dynamic fib3[n]
  (if (< n 2) n
      (+ (fib3 (dec n)) (fib3 (dec (dec n)))))) ; #'user/fib3

;; Still slow:

(time (fib3 30))
;; "Elapsed time: 1256.942478 msecs"
;; 832040

;; We get back some ability to trace:

(report (clojure.contrib.trace/dotrace (fib3) (fib3 5)))
;; TRACE t7877: (fib3 5)
;; TRACE t7877: => 5
;; 5

(def fib3 (memoize fib3)) ; #'user/fib3

;; Memoization kills it though
(report (clojure.contrib.trace/dotrace (fib3) (fib3 5)))
;; #<IllegalStateException java.lang.IllegalStateException: Can't dynamically bind non-dynamic var: user/fib3>

;; Although again it's evident that it must have worked
(time (fib3 30))
;; "Elapsed time: 7.610874 msecs"
;; 832040

;; If we both label the var as :dynamic, and make the recursive calls explicit with #'
(defn ^:dynamic fib4[n]
  (if (< n 2) n
      (+ (#'fib4 (dec n)) (#'fib4 (dec (dec n)))))) ; #'user/fib4

(time (fib4 30))
;; "Elapsed time: 1335.919594 msecs"
;; 832040

;; Then we can get our tracing back

(report (clojure.contrib.trace/dotrace (fib4) (fib4 5))) ; 
;; TRACE t8080: (fib4 5)
;; TRACE t8081: |    (fib4 4)
;; TRACE t8082: |    |    (fib4 3)
;; TRACE t8083: |    |    |    (fib4 2)
;; TRACE t8084: |    |    |    |    (fib4 1)
;; TRACE t8084: |    |    |    |    => 1
;; TRACE t8085: |    |    |    |    (fib4 0)
;; TRACE t8085: |    |    |    |    => 0
;; TRACE t8083: |    |    |    => 1
;; TRACE t8086: |    |    |    (fib4 1)
;; TRACE t8086: |    |    |    => 1
;; TRACE t8082: |    |    => 2
;; TRACE t8087: |    |    (fib4 2)
;; TRACE t8088: |    |    |    (fib4 1)
;; TRACE t8088: |    |    |    => 1
;; TRACE t8089: |    |    |    (fib4 0)
;; TRACE t8089: |    |    |    => 0
;; TRACE t8087: |    |    => 1
;; TRACE t8081: |    => 3
;; TRACE t8090: |    (fib4 3)
;; TRACE t8091: |    |    (fib4 2)
;; TRACE t8092: |    |    |    (fib4 1)
;; TRACE t8092: |    |    |    => 1
;; TRACE t8093: |    |    |    (fib4 0)
;; TRACE t8093: |    |    |    => 0
;; TRACE t8091: |    |    => 1
;; TRACE t8094: |    |    (fib4 1)
;; TRACE t8094: |    |    => 1
;; TRACE t8090: |    => 2
;; TRACE t8080: => 5
;; 5

;; And if we also mark the redefinition:

(def ^:dynamic fib4 (memoize fib4)) ; #'user/fib4

;; Then things seem to be working as before

(report (clojure.contrib.trace/dotrace (fib4) (fib4 5))) ; 
;; TRACE t8104: (fib4 5)
;; TRACE t8105: |    (fib4 4)
;; TRACE t8106: |    |    (fib4 3)
;; TRACE t8107: |    |    |    (fib4 2)
;; TRACE t8108: |    |    |    |    (fib4 1)
;; TRACE t8108: |    |    |    |    => 1
;; TRACE t8109: |    |    |    |    (fib4 0)
;; TRACE t8109: |    |    |    |    => 0
;; TRACE t8107: |    |    |    => 1
;; TRACE t8110: |    |    |    (fib4 1)
;; TRACE t8110: |    |    |    => 1
;; TRACE t8106: |    |    => 2
;; TRACE t8111: |    |    (fib4 2)
;; TRACE t8111: |    |    => 1
;; TRACE t8105: |    => 3
;; TRACE t8112: |    (fib4 3)
;; TRACE t8112: |    => 2
;; TRACE t8104: => 5
;; 5

(time (fib4 30)) ; "Elapsed time: 0.753562 msecs"
832040

;; Except that now, integer overflow has crept into our non-optimized version:

(report (time (fib4 100))) ; #<ArithmeticException java.lang.ArithmeticException: integer overflow>

;; To fix this, we need to explicitly make a number a bigint, and hope that it contaminates everything else

(defn ^:dynamic fib5[n]
  (if (< n 2) (bigint n)
      (+ (#'fib5 (dec n)) (#'fib5 (dec (dec n)))))) ; #'user/fib5

(def ^:dynamic fib5 (memoize fib5)) ; #'user/fib5

(report (time (fib5 30)))
;; "Elapsed time: 1.130838 msecs"
;; 832040N

;; So now we don't get integer overflows, but numbers have an N on the end.
(report (time (fib5 100))) 
;; "Elapsed time: 2.466388 msecs"
;; 354224848179261915075N

;;The stack still blows:
(report (time (fib5 300))) ; #<StackOverflowError java.lang.StackOverflowError>



;; Try the loop/recur version again
(defn fib-it [n]
  (loop [a 0 b 1 count 0]
               (if (< count n)
                   (recur b (+ a b) (inc count))
                   a))) ; #'user/fib-it

(time (fib-it 30)) ; 
;; "Elapsed time: 0.122168 msecs"
;; 832040

(report (time (fib-it 100))) ; #<ArithmeticException java.lang.ArithmeticException: integer overflow>

(report (time (fib-it 300)) ) ; #<ArithmeticException java.lang.ArithmeticException: integer overflow>

;; Try to optimize

(defn fib-it-opt [n]
  (let [cn (int n)]
    (loop [a (int 0) b (int 1) count (int 0)]
      (if (< count cn)
        (recur b (+ a b) (inc count))
        a)))) ; #'user/fib-it-opt

;; Doesn't seem to make a difference

(time (fib-it-opt 30))
;; "Elapsed time: 0.159273 msecs"
;; 832040

(report (time (fib-it-opt 300))) ; #<ArithmeticException java.lang.ArithmeticException: integer overflow>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Conclusion

;; It is not right to judge alpha versions of programs. But the fact that
;; they're public, labelled alphas might mean that they'll bear some resemblance
;; to the finished product.

;; But the issues raised here to do with tracing and rebinding and integer
;; overflow seem to come up all the time with the 1.3-alphas. And they translate
;; into hours of wasted debugging time and corner cases.

;; You have to ignore the timings in these little programs. They're hopelessly
;; uncontrolled.  But on the other hand, I haven't had much subjective
;; impression that Clojure's getting faster, and I do write a lot of clojure
;; code.

;; The one place where I did see an obvious speedup from 1.2 to 1.3 was my old
;; fractal tree program, one of the few cases where there didn't seem any way to
;; get Java speeds using clojure, but with 1.3-alpha4 I'm told that no longer
;; works unless you manually box and unbox the parameters, which presumably
;; kills off most of the speedup.

;; What does seem to be true is that the language is becoming complex and full
;; of gotchas.

;; Clojure 1.1 was a beautiful simple thing, from a programmer's perspective.

;; Clojure 1.2 had a few gotchas to do with when things could be rebound and
;; not, and tracing function calls didn't work so well. But nothing that
;; couldn't be lived with.

;; Clojure 1.3 (or at least the alpha versions so far) seems to make things more
;; difficult than they need to be, without as far as I can tell, obvious
;; benefits.

;; And I'm noticing that all the time in day to day programming.

;; Admittedly I mainly write mathematical code, so I'm probably not clojure's
;; target audience.

;; As I understand it, the primary goal of Clojure is performance on the JVM,
;; with simplicity as a secondary goal

;; The thing is, that in the cases when you actually have a tight loop that
;; needs to get faster, clojure makes it no trouble at all to shell out to java,
;; or to write a complicated form of java-like clojure that runs about the same
;; speed.

;; Either of these seem perfectly good options for getting speed.

;; Write nasty low level code where you really need it, everywhere else aim for
;; simplicity, abstraction and generality.

;; I would think that that was good advice whatever sort of code you were
;; writing.

;; The only place that I found that I couldn't get Java-like performance from
;; early versions of clojure was in the fractal tree drawing program, and of
;; course, the "shell out to java" option would still have been available, if
;; I'd cared enough to do it.

;; I stopped liking PLT scheme when they started changing the semantics of the
;; language in an attempt to get it to run faster. Without, as far as I could
;; tell, any noticeable success on the speeding up front.

;; I do hope clojure, which seemed like a promised land in its earlier versions,
;; and which I've been evangelical about, doesn't go the same way.

;; At the very least it would be interesting to know what the benefits of all
;; this new complexity are.


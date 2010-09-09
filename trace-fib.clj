;; This has always been one of my favourite lisp programs

(defn fib [n]
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(def fib (memoize fib))

;; In clojure 1.1, it worked the way that I expected it to:

;; fib is horribly slow as originally defined (exponential in n), but the
;; memoization makes it linear.

;; I also used to like to debug recursions by tracing them:

(use 'clojure.contrib.trace)
(dotrace (fib) (fib 5))

;; which would print out the input and output value of every recursive call

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In clojure 1.2, the above is broken. Tracing doesn't follow the recursive
;; calls, and memoization doesn't improve tree-recursions like fibonacci
;; (well, not the first time you run them anyway, which is kind of the point)

(defn fib [n]
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(dotrace (fib) (fib 5))

;; TRACE t2159: (fib 5)
;; TRACE t2159: => 5
;; 5

(time (fib 30)) ;; "Elapsed time: 1337.806227 msecs" 832040

(def fib (memoize fib))

(time (fib 30))  ;; "Elapsed time: 1396.284113 msecs" 832040
(time (fib 30))  ;; "Elapsed time: 0.181518 msecs" 832040
(time (fib 31))  ;; "Elapsed time: 2246.502303 msecs" 1346269

;; The problem in both cases is that the compiler has started to bind
;; the recursive calls earlier, avoiding the penalty for looking up
;; a variable that may have changed at run-time.

;; This is a big change to the semantics of the language.

;; We can, however, explicitly instruct it to do so:

(defn fib [n]
  (if (< n 2) n
      (+ ((resolve 'fib) (- n 1)) ((resolve 'fib) (- n 2)))))

(dotrace (fib) (fib 5))

(def fib (memoize fib))

(dotrace (fib) (fib 30))

;; Now things work fine!
;; I can feel a macro coming on ... 



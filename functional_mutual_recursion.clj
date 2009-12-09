;;As every schoolboy knows:

(defn factorial [n]
  (if (< n 2) n
      (* n (factorial (dec n)))))

(factorial 10)

(factorial 4500) ;; but 4750 blows the stack on my machine

;;We need a tail recursion

(defn factorial
  ( [n] 
      (factorial 1 n ))
  ( [acc n]
      (if (< n 2) (* n acc)
          (factorial (* n acc) (dec n)))))

(factorial 4750)

;;still busts the stack. sigh.

(defn factorial
  ( [n] 
      (factorial 1 n ))
  ( [acc n]
      (if (< n 2) (* n acc)
          (recur (* n acc) (dec n)))))

(factorial 10000) ;yay!

;;another way

(use 'clojure.contrib.repl-utils)
(source trampoline)

(defn trampoline
  "trampoline can be used to convert algorithms requiring mutual
  recursion without stack consumption. Calls f with supplied args, if
  any. If f returns a fn, calls that fn with no arguments, and
  continues to repeat, until the return value is not a fn, then
  returns that non-fn value. Note that if you want to return a fn as a
  final value, you must wrap it in some data structure and unpack it
  after trampoline returns."
  ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
  ([f & args]
     (trampoline #(apply f args))))

(defn factorial [n]
  (println "n=" n)
  (if (< n 2) n
      #(* n (factorial (dec n)))))


(trampoline factorial 3)

(factorial 3)
(#(* 3 (factorial 2)))
























;thunk = lambda name, *args: lambda: name(*args)

(defn thunk [name & args]
  (fn[] (apply name args)))

((thunk * 2 3))

;def _trampoline(bouncer):
;    while callable(bouncer):
;        bouncer = bouncer()
;    return bouncer


(defn _tramp [bouncer]
  (if (fn? bouncer) (bouncer)
      bouncer))

(_tramp (thunk * 2 3))

;trampoline = lambda f: lambda *args: _trampoline(f(*args))

(def tramp 
  (fn [f]
    (fn [ & args ] ( _tramp (apply f args)))))



;_factorial = lambda n, c=identity: c(1) if n == 0 else thunk(_factorial, n - 1, lambda result: thunk(c, n * result))
(defn _factorial 
  ([n, c]
    (if (= n 0) (c 1)
        (thunk _factorial (- n 1) (fn[result] (thunk c (* n result))))))
  ([n] (_factorial n identity)))

;factorial = trampoline(_factorial)
(def factorial (tramp _factorial))

print factorial(100000)











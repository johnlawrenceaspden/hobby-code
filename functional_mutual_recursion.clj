;;As any schoolboy knows:

(defn factorial [n]
  (if (< n 2) n
      (* n (factorial (dec n)))))

(factorial 10)

(factorial 4500) ;; 4750 blows the stack on my machine

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
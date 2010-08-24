;;output hello, append world, read it back

;;There's no line-by line reader function in clojure, so we have to make one

;;This is lazy, but I can't see how the file ever gets closed.
(require 'clojure.java.io)
(defn slurp-lines [f] (line-seq (clojure.java.io/reader f)))

;;However, this loop seems to run quite happily, so presumably file handles
;;are getting garbage collected.
(loop []
  (println (first (slurp-lines "hello.txt")))
  (recur))

(defn doom []
  (lazy-cons (slurp-lines "hello.txt")) (doom))

(take 10000 (map first doom))

;;have I always been able to do this?
(def fibs (lazy-seq (cons 0 (cons 1 (map + fibs (next fibs))))))

;;This is eager, reading the whole file in and closing it, which seems like
;;overkill for the second line.
(require 'clojure.string)
(defn slurp-lines [f] (clojure.string/split-lines (slurp f)))

(let [f "hello.txt"]
  (spit f "hello\n") 
  (spit f "world\n" :append true)
  (print (second (slurp-lines f))))

(doto "hello.txt"
  (spit "hello\n")
  (spit "world\n" :append true)
  (-> slurp-lines second print ))

(doto "hello.txt"
  (spit "hello\n")
  (spit "world\n" :append true)
  (#(print (second (slurp-seq %)))))


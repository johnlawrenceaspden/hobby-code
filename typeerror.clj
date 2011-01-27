;; I have twice recently been bitten by this:

;; You have a collection of things
(require 'clojure.string)

(def coll (clojure.string/split ";; I have twice recently been bitten by this: You have a collection of things" #"\s+"))

;; On which there is some function

(map count coll) ;(2 1 4 5 8 4 6 2 5 3 4 1 10 2 6)

;; And you'd like to get the elements with the smallest values

;; And so you end up writing
(defn take-n-smallest [n f coll]
  (map second
       (take n (sort (map (fn[s] [(f s) s]) coll)))))

(take-n-smallest 3 count coll) ; ("I" "a" ";;")


;; And this works absolutely peachily

;; And so you end up using it in other functions

(defn afunc [coll]
  (take-n-smallest 3 count coll)) ; #'user/afunc

(afunc coll) ; ("I" "a" ";;")

(defn bfunc [coll]
  (nth (afunc coll) 2)) ; #'user/bfunc

(bfunc coll) ; ";;"

;; And you come to rely on this function, which seems pretty robust

(bfunc ["the" "doom" "that" "came" "to" "Sarnath"]) ; "came"

(bfunc [[1] [1 2] [1 2 3] [1 2 3 4]]) ; [1 2 3]

(bfunc [{:what 2 :hath 3} {:God 3} {:Wrought 2 :in 3 :his 5}]) ; {:in 3, :Wrought 2, :his 5}

;; And then suddenly one day:

;; (bfunc [{:what 2 :hath 3} {:God 3} {:Wrought 2}]) ; Evaluation Aborted

;; To be more precise

(clojure.stacktrace/print-stack-trace
 (try
   (bfunc [{:what 2 :hath 3} {:God 3} {:Wrought 2}])
   (catch java.lang.Exception e e)))

;; java.lang.RuntimeException: java.lang.ClassCastException: clojure.lang.PersistentArrayMap cannot be cast to java.lang.Comparable
;;  at clojure.lang.AFunction.compare (AFunction.java:39)
;;     java.util.Arrays.mergeSort (Arrays.java:1283)
;;     java.util.Arrays.sort (Arrays.java:1223)
;;     clojure.core$sort.invoke (core.clj:2634)
;;     clojure.core$sort.invoke (core.clj:2630)
;;     user$take_n_smallest.invoke (NO_SOURCE_FILE:1)
;;     user$afunc.invoke (NO_SOURCE_FILE:1)
;;     user$bfunc.invoke (NO_SOURCE_FILE:1)
;;     user$eval9076$fn__9077.invoke (NO_SOURCE_FILE:1)
;;     user$eval9076.invoke (NO_SOURCE_FILE:1)
;;     clojure.lang.Compiler.eval (Compiler.java:6201)
;;     clojure.lang.Compiler.eval (Compiler.java:6168)
;;     clojure.core$eval.invoke (core.clj:2680)
;;     swank.commands.basic$eval_region.invoke (basic.clj:47)
;;     swank.commands.basic$eval_region.invoke (basic.clj:37)
;;     swank.commands.basic$eval2067$eval_and_grab_output__2068$fn__2069.invoke (basic.clj:82)
;;     swank.commands.basic$eval2067$eval_and_grab_output__2068.invoke (basic.clj:81)
;;     clojure.lang.Var.invoke (Var.java:401)
;;     user$eval9074.invoke (NO_SOURCE_FILE:-1)
;;     clojure.lang.Compiler.eval (Compiler.java:6201)
;;     clojure.lang.Compiler.eval (Compiler.java:6168)
;;     clojure.core$eval.invoke (core.clj:2680)
;;     swank.core$eval_in_emacs_package.invoke (core.clj:92)
;;     swank.core$eval_for_emacs.invoke (core.clj:239)
;;     clojure.lang.Var.invoke (Var.java:409)
;;     clojure.lang.AFn.applyToHelper (AFn.java:169)
;;     clojure.lang.Var.applyTo (Var.java:518)
;;     clojure.core$apply.invoke (core.clj:600)
;;     swank.core$eval_from_control.invoke (core.clj:99)
;;     swank.core$spawn_worker_thread$fn__1190$fn__1194.invoke (core.clj:298)
;;     clojure.lang.AFn.applyToHelper (AFn.java:159)
;;     clojure.lang.AFn.applyTo (AFn.java:151)
;;     clojure.core$apply.invoke (core.clj:600)
;;     swank.core$spawn_worker_thread$fn__1190.doInvoke (core.clj:294)
;;     clojure.lang.RestFn.invoke (RestFn.java:398)
;;     clojure.lang.AFn.run (AFn.java:24)
;;     java.lang.Thread.run (Thread.java:636)


;; Now as it happens, because I have twice recently had to find this exact same
;; bug, and because I have just constructed this example program to cause it, it
;; is pretty obvious to me what the bug is and how to fix it.

;; But on the two occasions where it happened, it wasn't at all obvious, and to
;; find it I had to manually recurse through my function calls.

;; This just wasn't pleasant.

;; So what to do instead?

;; Well the first tip is to either run the file from the command line
;; On my system with ubuntu and maven, the command to do this is:

;; $ java -cp /home/john/.m2/repository/org/clojure/clojure/1.3.0-alpha4/clojure-1.3.0-alpha4.jar:/home/john/.m2/repository/org/clojure/contrib/standalone/1.3.0-alpha4/standalone-1.3.0-alpha4.jar clojure.main typeerror.clj

;; This will produce a stack trace with the error locations properly labelled,
;; rather than the NO_SOURCE_FILE:1 above, which means that the code was
;; evaluated interactively

;; An alternative, if you're using swank/emacs, is to compile (C-c C-k) your files.
;; I usually evaluate all the definitions with (M-x), which leads to NO_SOURCE_FILE:1

;; Once you've compiled all the files, the stack traces are more helpful

(clojure.stacktrace/print-stack-trace
 (try
   (bfunc [{:what 2 :hath 3} {:God 3} {:Wrought 2}])
   (catch java.lang.Exception e e)))

;; java.lang.RuntimeException: java.lang.ClassCastException: clojure.lang.PersistentArrayMap cannot be cast to java.lang.Comparable
;;  at clojure.lang.AFunction.compare (AFunction.java:39)
;;     java.util.Arrays.mergeSort (Arrays.java:1283)
;;     java.util.Arrays.sort (Arrays.java:1223)
;;     clojure.core$sort.invoke (core.clj:2634)
;;     clojure.core$sort.invoke (core.clj:2630)
;;     user$take_n_smallest.invoke (typeerror.clj:17)
;;     user$afunc.invoke (typeerror.clj:27)
;;     user$bfunc.invoke (typeerror.clj:32)
;;     user$eval9406$fn__9407.invoke (NO_SOURCE_FILE:1)
;;     user$eval9406.invoke (NO_SOURCE_FILE:1)
;;     clojure.lang.Compiler.eval (Compiler.java:6201)
;;     clojure.lang.Compiler.eval (Compiler.java:6168)
;;     clojure.core$eval.invoke (core.clj:2680)
;;     swank.commands.basic$eval_region.invoke (basic.clj:47)
;;     swank.commands.basic$eval_region.invoke (basic.clj:37)
;;     swank.commands.basic$eval2067$eval_and_grab_output__2068$fn__2069.invoke (basic.clj:82)
;;     swank.commands.basic$eval2067$eval_and_grab_output__2068.invoke (basic.clj:81)
;;     clojure.lang.Var.invoke (Var.java:401)
;;     user$eval9404.invoke (NO_SOURCE_FILE:-1)
;;     clojure.lang.Compiler.eval (Compiler.java:6201)
;;     clojure.lang.Compiler.eval (Compiler.java:6168)
;;     clojure.core$eval.invoke (core.clj:2680)
;;     swank.core$eval_in_emacs_package.invoke (core.clj:92)
;;     swank.core$eval_for_emacs.invoke (core.clj:239)
;;     clojure.lang.Var.invoke (Var.java:409)
;;     clojure.lang.AFn.applyToHelper (AFn.java:169)
;;     clojure.lang.Var.applyTo (Var.java:518)
;;     clojure.core$apply.invoke (core.clj:600)
;;     swank.core$eval_from_control.invoke (core.clj:99)
;;     swank.core$spawn_worker_thread$fn__1190$fn__1194.invoke (core.clj:298)
;;     clojure.lang.AFn.applyToHelper (AFn.java:159)
;;     clojure.lang.AFn.applyTo (AFn.java:151)
;;     clojure.core$apply.invoke (core.clj:600)
;;     swank.core$spawn_worker_thread$fn__1190.doInvoke (core.clj:294)
;;     clojure.lang.RestFn.invoke (RestFn.java:398)
;;     clojure.lang.AFn.run (AFn.java:24)
;;     java.lang.Thread.run (Thread.java:636)


(def st (seq (.getStackTrace (try
                               (bfunc [{:what 2 :hath 3} {:God 3} {:Wrought 2}])
                               (catch java.lang.Exception e e)))))


(map #(.getFileName %) st) ; ("AFunction.java" "Arrays.java" "Arrays.java" "core.clj" "core.clj" "typeerror.clj" "typeerror.clj" "typeerror.clj" "NO_SOURCE_FILE" "AFn.java" "AFn.java" "Compiler.java" "Compiler.java" "Compiler.java" "Compiler.java" "Compiler.java" "Compiler.java" "core.clj" "basic.clj" "basic.clj" "basic.clj" "Var.java" "NO_SOURCE_FILE" "Compiler.java" "Compiler.java" "core.clj" "core.clj" "core.clj" "Var.java" "AFn.java" "Var.java" "core.clj" "core.clj" "core.clj" "AFn.java" "AFn.java" "core.clj" "core.clj" "RestFn.java" "AFn.java" "Thread.java")

;; If we can get the file names, then we can filter by them

(defn interesting[ste]
  (let [fname (.getFileName ste)]
    (or
     (= (.toUpperCase fname) fname)
     (re-matches #".*\.clj" fname))))

(map #(.getFileName %) (filter interesting st)) ; ("core.clj" "core.clj" "typeerror.clj" "typeerror.clj" "typeerror.clj" "NO_SOURCE_FILE" "core.clj" "basic.clj" "basic.clj" "basic.clj" "NO_SOURCE_FILE" "core.clj" "core.clj" "core.clj" "core.clj" "core.clj" "core.clj" "core.clj" "core.clj")

;; Errors are unlikely to be in core.clj or basic.clj (part of SWANK)

(defn very-interesting[ste]
  (let [fname (.getFileName ste)]
    (and 
     (or
      (= (.toUpperCase fname) fname)
      (re-matches #".*\.clj" fname))
     (not(#{ "core.clj" "basic.clj"} fname)))))

(map #(.getFileName %) (filter very-interesting st)) ; ("typeerror.clj" "typeerror.clj" "typeerror.clj" "NO_SOURCE_FILE" "NO_SOURCE_FILE")

;; These look interesting, so

(map (fn[e] [(.getFileName e), (.getLineNumber e), (.getClassName e)]) (filter very-interesting st))
                                        ; (["typeerror.clj" 17 "user$take_n_smallest"]
                                        ;  ["typeerror.clj" 27 "user$afunc"]
                                        ;  ["typeerror.clj" 32 "user$bfunc"]
                                        ;  ["NO_SOURCE_FILE" 1 "user$fn__10301"]
                                        ;  ["NO_SOURCE_FILE" -1 "user$eval10299"])

;; The first one (the deepest clojure file written by me which has thrown an error)
;; Is probably the source of the error


;; Here is the source for take-n-smallest
(defn take-n-smallest [n f coll]
  (map second
       (take n (sort (map (fn[s] [(f s) s]) coll)))))  ;; this is line 17


;; We can debug it so:
(defn take-n-smallest [n f coll]
  (println n f coll)
  (map second
       (take n (sort (map (fn[s] [(f s) s]) coll)))))

;; Remembering to re-evaluate everything that depends on that!


(try
  (bfunc [{:what 2 :hath 3} {:God 3} {:Wrought 2}]) ; 
  (catch java.lang.Exception e e))
;; 3 #<core$count clojure.core$count@11ef005> [{:hath 3, :what 2} {:God 3} {:Wrought 2}]
;; #<RuntimeException java.lang.RuntimeException: java.lang.ClassCastException: clojure.lang.PersistentArrayMap cannot be cast to java.lang.Comparable>

;; So that's isolated our error to be

(try
  (take-n-smallest 3 count [{:hath 3, :what 2} {:God 3} {:Wrought 2}])
  (catch Exception e e))
;; 3 #<core$count clojure.core$count@11ef005> [{:hath 3, :what 2} {:God 3} {:Wrought 2}]
;; #<RuntimeException java.lang.RuntimeException: java.lang.ClassCastException: clojure.lang.PersistentArrayMap cannot be cast to java.lang.Comparable>

;; So now we can do a bit of hand-evaluating
(try 
  (map second (take 3 (sort (map (fn[s] [(count s) s]) [{:hath 3, :what 2} {:God 3} {:Wrought 2}]))))
  (catch Exception e e))
;; #<RuntimeException java.lang.RuntimeException: java.lang.ClassCastException: clojure.lang.PersistentArrayMap cannot be cast to java.lang.Comparable>

(try 
  (map (fn[s] [(count s) s]) [{:hath 3, :what 2} {:God 3} {:Wrought 2}])
  (catch Exception e e))
;; ([2 {:hath 3, :what 2}] [1 {:God 3}] [1 {:Wrought 2}])


(try 
  (map second (sort '([2 {:hath 3, :what 2}] [1 {:God 3}] [1 {:Wrought 2}])))
  (catch Exception e e)) ; #<RuntimeException java.lang.RuntimeException: java.lang.ClassCastException: clojure.lang.PersistentArrayMap cannot be cast to java.lang.Comparable>

(try 
  (sort '([2 {:hath 3, :what 2}] [1 {:God 3}] [1 {:Wrought 2}]))
  (catch Exception e e)) ; #<RuntimeException java.lang.RuntimeException: java.lang.ClassCastException: clojure.lang.PersistentArrayMap cannot be cast to java.lang.Comparable>


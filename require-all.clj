;; It often irritates me that I can't use clojure's built in documentation unless I've already
;; loaded the library I need.  

;; Sometimes it turns out that the generic-looking function I need already exists, but it can be
;; hard to find it.  Sometimes I can remember the function I want, but not its precise name or
;; library namespace.

;; It's also often the case that the best way to understand a function is to read the source.

;; The other day it occurred to me that a function to find all the namespaces available on the
;; classpath might help quite a bit, and I started to write one. And at that point I had the obvious
;; recursive thought.

;; Seek and ye shall find. Thank you Stuart Sierra.
(use 'clojure.contrib.find-namespaces)

;; First let's see what our classpath is:
(println "Classpath")
(doseq [p (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))] (println (str p)))

;; In my case, that's the clojure.jar and clojure.contrib.jar files, and also the directory
;; containing the files for swank-clojure, the library which allows it to talk to emacs.
(println "Swank namespaces")
(println (for [s (for [n (all-ns)] (name (ns-name n))) :when (. s contains "swank")] s))

;; Because I'm still a beginner, and not brave enough to modify swank, I'm more interested in 
;; the namespaces provided by clojure and clojure.contrib

;; So far, the following have managed to get themselves loaded:
(println "None-swank namespaces")
(use 'clojure.contrib.pprint)
(pprint (sort (for [s (for [n (all-ns)] (name (ns-name n))) :when (not (. s contains "swank"))] s)))

;; But the following are in fact available
(pprint (find-namespaces-on-classpath))

;; Again, let's ignore the swank-related ones.
(pprint (filter #(not (. (str %) startsWith "swank")) (find-namespaces-on-classpath)))

;;For me there are only namespaces starting with either swank or clojure, because I don't have much
;;of a classpath. But it's easy enough to check that.
(defn find-namespaces-starting-with [strng]
  (filter #(. (str %) startsWith strng) (find-namespaces-on-classpath)))

(map count (map find-namespaces-starting-with '("clojure" "swank" ""))) ; should add up

;; Some of the clojure.contrib libraries fail to load for me, so we have to bullet-proof require
(defn require-may-fail [ns]
  (try
   (print "Attempting to require " ns ": ")
   (require ns)
   (println "success")
   (catch Exception e (println "couldn't require " ns "\nException\n" e "\n\n"))))

;; Now we can load everything
(doall (map require-may-fail (filter #(. (str %) startsWith "clojure") (find-namespaces-on-classpath))))

;; Now we have lots and lots of namespaces available: The number of 'batteries included' is impressive.
(println "None-swank namespaces")
(pprint (sort (for [s (for [n (all-ns)] (name (ns-name n))) :when (not (. s contains "swank"))] s)))

;; And the find-doc, doc, and source commands become really useful
;; Of course we not only have to find source, we actually can, now.
(find-doc "source code")

;; Also note that all this requiring hasn't brought any new functions into play yet.
;; To use something without qualification, you need to refer as well.
(refer 'clojure.contrib.repl-utils)
(doc source)
(source source)
(source get-source)
(:file (meta (resolve 'source)))


;; Other sources of enjoyment are the find and grep commands, and browsing clojure.contrib.jar and
;; clojure.jar These work particularly nicely under emacs. A good arrangement of find and grep is
;; (assuming that your clojure sources are under ~/opt):

;; find ~/opt/ -name "*.clj" -and -type f -print0 | xargs -0 -e grep -nH -e "repl-util"

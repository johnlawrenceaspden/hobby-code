(ns hello)

(defn hello []
  (println "hello, maven")
  (println "sigh."))



(hello)

;;Trying to find out how to load the hello namespace into the repl and swank server
;;maven starts
(println "None-swank namespaces")
(println (for [s (for [n (all-ns)] (name (ns-name n))) :when (not (. s contains "swank"))] s))
(println "Swank namespaces")
(println (for [s (for [n (all-ns)] (name (ns-name n))) :when (. s contains "swank")] s))
(println "Classpath")
(println (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))
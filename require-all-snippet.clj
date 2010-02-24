;; Here's just the code needed to require all the namespaces on the classpath.
(require 'clojure.contrib.find-namespaces)

(defn require-may-fail [ns]
  (try
   (print "Attempting to require " ns ": ")
   (require ns)
   (println "success")
   (catch Exception e (println "couldn't require " ns "\nException\n" e "\n\n"))))

(defn load-all-namespaces []
  (doall (map require-may-fail 
              (filter #(. (str %) startsWith "clojure") 
                      (clojure.contrib.find-namespaces/find-namespaces-on-classpath)))))

(defn condition-repl[]
  (load-all-namespaces)
  (use 'clojure.contrib.repl-utils))

(condition-repl)




;; Here's just the code needed to require all the namespaces on the classpath.

(use 'clojure.contrib.find-namespaces)

(defn require-may-fail [ns]
  (try
   (print "Attempting to require " ns ": ")
   (require ns)
   (println "success")
   (catch Exception e (println "couldn't require " ns "\nException\n" e "\n\n"))))


(doall (map require-may-fail (filter #(. (str %) startsWith "clojure") (find-namespaces-on-classpath))))


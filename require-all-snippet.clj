;; LOAD ME WITH
;; (load-file "/home/john/hobby-code/require-all-snippet.clj")

;; Here's just the code needed to require all the namespaces on the classpath.
(require 'clojure.contrib.find-namespaces)

(defn require-may-fail [ns]
  (try
   (print "Attempting to require " ns ": ")
   (require ns)
   (println "success")
   (catch Exception e (println "couldn't require " ns "\nException\n" e "\n\n"))))

(defn require-all-namespaces-starting-with [strng]
  (doall (map require-may-fail 
              (filter #(. (str %) startsWith strng) 
                      (clojure.contrib.find-namespaces/find-namespaces-on-classpath)))))

;;use various handy utilities
(use 'clojure.contrib.repl-utils)
(use 'clojure.inspector)
(use 'clojure.set)
(use 'clojure.contrib.find-namespaces)
(use 'clojure.contrib.pprint)
(use 'clojure.contrib.repl-utils)
(use 'clojure.contrib.trace)

;;ways of asking what's available in a namespace
(defn ns-publics-list [ns] (#(list (ns-name %) (map first (ns-publics %))) ns))
(defn ns-refers-list  [ns] (#(list (ns-name %) (map first (ns-refers %))) ns))

(defn list-refers    
  ([]   (pprint (ns-refers-list *ns*)))
  ([ns] (pprint (ns-refers-list (find-ns ns)))))

(defn list-publics     
  ([]   (pprint (ns-publics-list *ns*)))
  ([ns] (pprint (ns-publics-list (find-ns ns)))))

(defn list-ns [] (pprint (map ns-name (all-ns))))
(defn list-publics-all-ns [] (pprint (map #(list (ns-name %) (map first (ns-publics %))) (all-ns))))

;;find symbol in docs 
(defmacro fd [sym] `(find-doc (str '~sym)))

;;debugging macro 
(defmacro dbg [x] `(let [x# ~x] (do (println '~x "->" x#) x#))) 
;;and pretty-printing version 
(defmacro ppdbg [x]`(let [x# ~x] (do (println "--")(pprint '~x)(println "->")(pprint x#) (println "--") x#))) 

;;and one for running tests 
(defmacro run-test [fn] `(test (resolve '~fn)))

;;like find-doc, but just the names
(defn find-doc-names
  "Prints the name of any var whose documentation or name contains a match for re-string-or-pattern"
  [re-string-or-pattern]
    (let [re  (re-pattern re-string-or-pattern)]
      (doseq [ns (all-ns)
              v (sort-by (comp :name meta) (vals (ns-interns ns)))
              :when (and (:doc ^v)
                         (or (re-find (re-matcher re (:doc ^v)))
                             (re-find (re-matcher re (str (:name ^v))))))]
               (print v "\n"))))

(defn print-classpath []
  (clojure.contrib.pprint/pprint 
   (sort (map (memfn getPath) 
              (seq (.getURLs 
                        (java.lang.ClassLoader/getSystemClassLoader)))))))


(defn condition-repl[]
  ;;require everything (so that find-doc can find it)
  (require-all-namespaces-starting-with "clojure"))

(condition-repl)

(println "Classpath:")
(print-classpath)

(println "Current Namespace")
(list-publics)


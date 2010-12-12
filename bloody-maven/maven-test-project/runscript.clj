(println "running the code from runscript.clj")

;script code:  intended to be the target of mvn clojure:run, clojure:repl, clojure:swank

(defn get-classpath-urls []
  (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))

(defn path-strings-from-urls [urls]
  (sort (map (memfn getPath) urls)))

(defn filter-namespaces [pred?]
  (for [s (for [n (all-ns)] (name (ns-name n))) :when (pred? s)] s))

(defn all-namespaces [] (filter-namespaces (fn[s] true)))

(defn all-non-swank-namespaces [] (filter-namespaces (fn[s] (not (. s contains "swank")))))

(defn banner [& s]
  (println "-----------------------------------------------------------")  
  (apply println s)
  (println "-----------------------------------------------------------"))

(defn get-public-symbols [ns] (map first (ns-publics ns)))
(defn get-all-symbols    [ns] (map first (ns-interns ns)))
(defn get-non-test-public-symbols [ns] (filter #(not (. (name %) startsWith "test-")) (map first (ns-publics ns))))

(defn pretty-print-list [list] (println (interpose "\n" (sort list))))

(defn print-current-environment []
  (banner "The classpath:")
  (pretty-print-list (path-strings-from-urls (get-classpath-urls)))

  (banner "all known namespaces")
  (pretty-print-list (all-namespaces))

  (banner "all the namespaces which do not have swank (the emacs interface) in the name.")
  (pretty-print-list (all-non-swank-namespaces))

  (banner (str "current namespace: [ " *ns* " ]"))
  (println "all symbols")
  (println (get-all-symbols *ns*))
  (println "public symbols")
  (println (get-public-symbols *ns*)))

;;sanity check to tell what we have loaded in an uncontaminated REPL (plus the functions we've just defined)
(print-current-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We can use the classes defined lower in the tree:

;; import the java class 
(import com.aspden.App)

;; use it
(time (App/factorial 12))
(time (App/factorial 123))

;; import the clojure namespace and use its factorial function
(require ['com.aspden.app :as 'caa])
(time (caa/factorial 12))
(time (caa/factorial 123.0))
(time (caa/factorial 123))

;; import the clojure-generated class and use its functions
(import com.aspden.app)
(def a (new com.aspden.app))
(class a)
(into [] (. (class a) getMethods))
(time (. a factorial 10))
(time (. a factorial 100))
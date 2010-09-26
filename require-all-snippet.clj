;; LOAD ME WITH
;; (load-file "/home/john/hobby-code/require-all-snippet.clj")

;; This file conditions a repl in various ways that I do all the time.

;; Firstly we want to 'require' all the namespaces on the classpath
;; This ensures that find-doc and the like will work
(require 'clojure.contrib.find-namespaces)

;; Some namespaces may fail to load, so catch any exceptions thrown
(defn- require-may-fail [ns]
  (try
   (print "Attempting to require " ns ": ")
   (require ns)
   (println "success")
   (catch Throwable e (println "couldn't require " ns "\nException\n" e "\n\n"))))

;; Generally we'd want clojure.*, clojure.contrib.*, and any project-specific namespaces
(defn require-all-namespaces-starting-with [strng]
  (doall (map require-may-fail 
              (filter #(. (str %) startsWith strng) 
                      (clojure.contrib.find-namespaces/find-namespaces-on-classpath)))))

;; Some of clojure's extra namespaces are so useful at the REPL that I want them
;; to have shorter names, i.e. I want to be able to type 'r/source' rather than
;; 'clojure.repl/source'.
;; This also means that emacs tab completion can find them with e.g. r/<TAB>

(require '(clojure [test :as t]
                   [inspector :as i]
                   [repl :as r]
                   [pprint :as pp])) 
(require '(clojure.contrib
           [trace :as cct]
           [repl-utils :as ccr]))


;; It drives me up the wall that it's (doc re-pattern) but (find-doc "re-pattern").
;; Can use macros so that (fd re-pattern) (fd "re-pattern") and (fd 're-pattern) all mean the same thing
(defn stringify [x]
  (println "stringify given" (str x))
  (let [s  (cond (string? x) x
                 (symbol? x) (str x)
                 (and (list? x) (= (first x) 'quote)) (str (second x))
                 :else (str x)) ]
    (println (str "translating to: \"" s "\""))
    s))


;; Sometimes I like to ask which public functions a namespace provides.
(defn ns-publics-list [ns] (#(list (ns-name %) (map first (ns-publics %))) ns))
;; And occasionally which functions it pulls in (with refer or use)
(defn ns-refers-list  [ns] (#(list (ns-name %) (map first (ns-refers %))) ns))

;; Nice pretty-printed versions of these functions, accepting strings, symbols or quoted symbol
(defmacro list-publics     
  ([]   `(clojure.pprint/pprint (ns-publics-list *ns*)))
  ([symbol-or-string] `(clojure.pprint/pprint (ns-publics-list (find-ns (symbol (stringify '~symbol-or-string)))))))

(defmacro list-refers
  ([]   `(clojure.pprint/pprint (ns-refers-list *ns*)))
  ([symbol-or-string] `(clojure.pprint/pprint (ns-refers-list (find-ns (symbol (stringify '~symbol-or-string)))))))

;; List all the namespaces
(defn list-all-ns [] (clojure.pprint/pprint (sort (map ns-name (all-ns)))))
;; List all public functions in all namespaces!
(defn list-publics-all-ns [] (clojure.pprint/pprint (map #(list (ns-name %) (map first (ns-publics %))) (all-ns))))

;; With all the namespaces loaded, find-doc can be overwhelming.
;; This is like find-doc, but just gives the associated names.
(defn find-doc-names
  "Prints the name of any var whose documentation or name contains a match for re-string-or-pattern"
  [re-string-or-pattern]
    (let [re  (re-pattern re-string-or-pattern)]
      (doseq [ns (all-ns)
              v (sort-by (comp :name meta) (vals (ns-interns ns)))
              :when (and (:doc (meta v))
                         (or (re-find (re-matcher re (:doc (meta v))))
                             (re-find (re-matcher re (str (:name (meta v)))))))]
               (print v "\n"))))




;;find symbol or string in docs 
(defmacro fd [symbol-or-string] `(find-doc (stringify '~symbol-or-string)))

(defmacro fdn [symbol-or-string] `(find-doc-names (stringify '~symbol-or-string)))

(defn source-file* [symbol] (:file (meta (resolve symbol))))

(defmacro source-file [symbol-or-string] `(source-file* (symbol (stringify '~symbol-or-string))))

;;get the methods of a java object
(defn meths [x] (println (apply str (interpose "\n" (map str (.getMethods (if (class? x) x (class x))))))))


;;debugging macro                                try: (* 2 (dbg (* 3 4)))
(defmacro dbg [x] `(let [x# ~x] (do (println '~x "->" x#) x#))) 
;;and pretty-printing version 
(defmacro ppdbg [x]`(let [x# ~x]
                      (do (println "--")
                          (clojure.pprint/pprint '~x)
                          (println "->")
                          (clojure.pprint/pprint x#)
                          (println "--") x#))) 

;;and one for running tests 
(defmacro run-test [fn] `(test (resolve '~fn)))

;; def-let as in blogpost
(defmacro def-let
  "like let, but binds the expressions globally."
  [bindings & more]
  (let [let-expr (macroexpand `(let ~bindings))
        names-values (partition 2 (second let-expr))
        defs   (map #(cons 'def %) names-values)]
    (concat (list 'do) defs more)))


;; Sometimes it's nice to check the classpath
(defn- get-classpath []
   (sort (map (memfn getPath) 
              (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))))

(defn print-classpath []
  (clojure.pprint/pprint (get-classpath)))

(defn get-current-directory []
  (. (java.io.File. ".") getCanonicalPath))

;;print the classpath
(println "Classpath:")
(print-classpath)

(println "Current Directory" (get-current-directory))

;;print the public functions in the current namespace
(println "Current Namespace")
(list-publics)

;;hint on how to require project specific namespaces
(println "to require all namespaces starting with example:")
(println "(require-all-namespaces-starting-with \"example\")")

;; see http://blog.n01se.net/?p=85
(println "setting *print-length* to 103, *print-level* to 13 to deal with infinities")
(set! *print-length* 103)
(set! *print-level* 13)

;; but we don't need this bit, 
;; (require 'clojure.contrib.repl-utils)
;; (clojure.contrib.repl-utils/add-break-thread!)
;; because swank repl threads already have a break handler set.
;; might come in useful for command line repls though.


;;require everything from clojure and clojure.contrib, so that find-doc can find it. Do it in an agent so it doesn't block repl startup. Jesus, am I really allowed to do this?
(def require-all-agent (agent "not done"))
(send-off require-all-agent (fn[agent] (with-out-str (require-all-namespaces-starting-with "clojure"))))


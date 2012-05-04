;; LOAD ME WITH
;; (load-file "/home/likestream/hobby-code/u.clj")

;; This file conditions a repl in various ways that I do all the time.

(ns u)

(require 'clojure.pprint)

;; This file conditions a repl in various ways that I do all the time.

;; Some of clojure's extra namespaces are so useful at the REPL that I want them
;; to have shorter names, i.e. I want to be able to type 'r/source' rather than
;; 'clojure.repl/source'.
;; This also means that emacs tab completion can find them with e.g. r/<TAB>

;;bring useful clojure namespaces into your current namespace with short names
(defn sn[]
  "require clojure.contrib.trace as cct, etc"
  (require '(clojure [test :as ct]
                     [inspector :as ci]
                     [repl :as cr]
                     [pprint :as pp]
                     [string :as cs]
                     ))
  (require '[clojure.java.javadoc :as cjj])
  (require '[clojure.tools.trace :as ctt])
  (require '[clojure.repl :as cr]))

;; In order to be able to use find-doc and tab-completion properly,
;; we want to require all the namespaces that we can find.

;; Stuart Sierra wrote a namespace finder:
(require '[clojure.tools.namespace :as ctn])

;; But namespaces often have errors, and fail to load, so we need to catch any exceptions thrown,
;; as well as reporting progress to the console
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
                      (ctn/find-namespaces-on-classpath)))))

;; Requiring everything from clojure and clojure.contrib, so that find-doc can find it.
;; This takes a while, so rather than blocking the REPL/swank startup while it happens, we can start an agent to do it in the background.
;; I am still not entirely certain of the wisdom of this approach (Initial comment was 'Jesus, am I really allowed to do this?')
;; But it's never caused any trouble and is better than the alternatives of not doing this, or waiting for it to complete every time.
(def require-all-agent (agent "not done"))
(send-off require-all-agent (fn[agent] (with-out-str (require-all-namespaces-starting-with "clojure"))))


;; It drives me up the wall that it's (doc re-pattern) but (find-doc "re-pattern").
;; We can use a macro so that (fd re-pattern) (fd "re-pattern") and (fd 're-pattern) all mean the same thing
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
(defmacro fd [symbol-or-string] `(clojure.repl/find-doc (stringify '~symbol-or-string)))

(defmacro fdn [symbol-or-string] `(find-doc-names (stringify '~symbol-or-string)))

;; find the source file which defines a thing:
(defn source-file* [symbol] (:file (meta (resolve symbol))))

(defmacro source-file [symbol-or-string] `(source-file* (symbol (stringify '~symbol-or-string))))

;;get the methods of a java object
(defn meths [x] (println (apply str (interpose "\n" (map str (.getMethods (if (class? x) x (class x))))))))
;;get just the names of the methods
(defn meth-names[x] (map #(.getName %) (.getMethods (if (class? x) x (class x)))))


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
(defn classpath []
   (sort (map (memfn getPath) 
              (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))))

;; or current directory
(defn current-directory []
  (. (java.io.File. ".") getCanonicalPath))

;; I often find myself restarting clojure in order to check that there aren't
;; any old definitions hanging about.  This is sort-of-equivalent.
(defn shred-ns [ns-symbol]
  (doseq [s (map first (ns-interns ns-symbol))](ns-unmap ns-symbol s)))


;;print the classpath
(println "Classpath:")
(clojure.pprint/pprint (classpath))

(println "Current Directory:" (current-directory))

;;print the public functions in the current namespace
(println "Current Namespace")
(list-publics)

;;hint on how to require project specific namespaces
(println "to require all namespaces starting with example:")
(println "(require-all-namespaces-starting-with \"example\")")

;; see http://blog.n01se.net/?p=85
(println "setting *print-length* to 103, *print-level* to 103 to deal with infinities")
(println "you have to do this before starting the swank server if you want it to work in emacs evaluations")
(set! *print-length* 103)
(set! *print-level* 103)

;; but we don't need this bit, 
;; (require 'clojure.contrib.repl-utils)
;; (clojure.contrib.repl-utils/add-break-thread!)
;; because swank repl threads already have a break handler set.
;; might come in useful for command line repls though.






;;ls --reverse --time
(defn lsrt
  ([] (lsrt 10 "."))
  ([n dirstr]
  (map second (take n (reverse (sort (map #(vector (java.util.Date. (.lastModified %))(.getName %) ) (seq (.listFiles (java.io.File. dirstr))))))))))

;; Convert an exception to a return value, even if it's the sort of exception that only happens when you
;; try to print the sequence.
(defmacro tryc[ & e]
  `(try (let [r# ~@e]
          (if (seq? r#)
            (doall r#)
            r#))
          (catch Throwable a# a#)))


(tryc (filter #(/ 1 %) '( 3 2 1 0))) ; #<RuntimeException java.lang.RuntimeException: java.lang.ArithmeticException: Divide by zero>
(tryc (filter #(/ 1 %) '( 3 2 1 1))) ; (3 2 1 1)
(tryc (/ 1 0)) ; #<ArithmeticException java.lang.ArithmeticException: Divide by zero>
(tryc (/ 1 1)) ; 1

;; And also I find some of the errors rather mystifying, so I decided to keep a list of hints towards common causes 
(defn interpret [e]
  (cond (and
         (= (class e) java.lang.ClassCastException)
         (= (.getMessage e) "java.lang.Class cannot be cast to clojure.lang.IFn"))
        "Possibly you forgot a dot.\n(java.Class arg) rather than\n(java.Class. arg)\n can cause this."
        (and
         (= (str (class e)) "class com.rabbitmq.client.MalformedFrameException")
         (re-matches #"AMQP protocol version mismatch; we are version .*, server is .*" (.getMessage e)))
        "RabbitMQ version mismatch error",
        ;; but I've only got two so far
        :else
        "Confucius he largely mystified by this.")) ; so my guru is not as helpful as he could be

(defn interpret-exception [a]
  (loop [a a]
    (let [type (class a)
          message (.getMessage a) 
          st (seq (.getStackTrace a))
          unusual (filter #(let [filename (.getFileName %)
                                 classname (.getClassName %)]
                             (and (not (#{"basic.clj" "core.clj"} filename))
                                  (not (re-matches #".*\.java" filename))
                                  (not (re-matches #"clojure\.lang.*" classname))))
                          st)]
      ;; print formatted
      (println type)
      (println message)
      (println "--")
      (clojure.pprint/pprint (map #(vector (.getFileName %)(.getLineNumber %)(.getClassName %)) unusual))
      (println "--\n" (interpret a)))
    (when-let[cause (.getCause a)]
      (println "cause:")
      (interpret-exception cause))))


;; Exception interpretation AI
(defmacro guru[ & e]
  `(try (let [r# ~@e]
          (if (seq? r#)
            (doall r#)
            r#))
        (catch Throwable e#
          (interpret-exception e#)
          e#)))




;; lein repl takes 52 seconds to start with this project file

(defproject hobby-code "0.0.1"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.trace "0.7.5"]
                 [org.clojure/tools.reader "0.7.0"]
                 [org.clojure/core.logic "0.8.1"]
                 [com.cemerick/pomegranate "0.0.13"]
		 [simple-plotter "0.1.2"]

 ;                [org.clojars.achim/multiset "0.1.0-SNAPSHOT"]
                 ]

;; The magic words are:
;; (require 'cemerick.pomegranate)
;; (cemerick.pomegranate/add-dependencies 
;;  :coordinates '[[ring "1.1.7"]]
;;  :repositories {"clojars" "http://clojars.org/repo" } )

;; (cemerick.pomegranate/add-dependencies :coordinates '[[org.clojure/tools.trace "0.7.5"]])

  :jvm-opts ["-Xmx900M"] 	
  :repl-options { 
                 :port 4001 
                 :init (do (println "hello from hobby-code/project.clj")
                           ;(set! *print-length* 103)
                           ;(set! *print-level* 13)
                           (alter-var-root (var *print-length*) (fn[f] 103))
                           (alter-var-root (var *print-level*) (fn[f] 13))

                           (defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

                           (defmacro def-let
                             "like let, but binds the expressions globally."
                             [bindings & more]
                             (let [let-expr (macroexpand `(let ~bindings))
                                   names-values (partition 2 (second let-expr))
                                   defs   (map #(cons 'def %) names-values)]
                               (concat (list 'do) defs more)))
                           (println "defined user/def-let and user/dbg, set! *print-level* and *print-length*"))
                 }
  :source-paths ["."]
  :min-lein-version "2.0.0"
)


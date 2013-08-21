;; The magic words for pomegranate are:

;; (require 'cemerick.pomegranate)
;; for something in maven central:
;; (cemerick.pomegranate/add-dependencies :coordinates '[[org.clojure/tools.trace "0.7.5"]])
;; for something in clojars
;; (cemerick.pomegranate/add-dependencies   :coordinates '[[ring "1.1.7"]] :repositories {"clojars" "http://clojars.org/repo" } )

{:user 
 {:min-lein-version "2.0.0"
  :source-paths ["/home/john/.lein/globaluser"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [nrepl-inspect "0.3.0"]
                 [ritz/ritz-nrepl-middleware "0.7.0"]
                 [com.cemerick/pomegranate "0.0.13"]
                 [org.clojure/java.classpath "0.2.0"]
                 [org.clojure/tools.namespace "0.2.3"]
                 [org.clojure/tools.trace "0.7.5"]
                 [org.clojure/tools.reader "0.7.0"]
                 [simple-plotter "0.1.2"]]
  :plugins [[lein-ritz "0.7.0"]
            [lein-clojars "0.9.1"]
            [lein-pprint "1.1.1"]]
  :repl-options  { 
                  :port 4001 
                  :init (do (println "hello from ~/.lein/profiles.clj"))
                  }
  :nrepl-middleware [inspector.middleware/wrap-inspect
                     ritz.nrepl.middleware.javadoc/wrap-javadoc
                     ritz.nrepl.middleware.apropos/wrap-apropos]}}

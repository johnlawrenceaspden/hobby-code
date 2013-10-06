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
                 ;; Things that are only for development use
                 [nrepl-inspect "0.3.0"]
                 [ritz/ritz-nrepl-middleware "0.7.0"]
                 [com.cemerick/pomegranate "0.0.13"]
                 [org.clojure/tools.trace "0.7.5"]
                 [org.clojure/java.classpath "0.2.0"]
                 [org.clojure/tools.namespace "0.2.3"]
                 [simple-plotter "0.1.2"]
                 ;; Very useful things
                 [org.clojure/tools.reader "0.7.0"]
                 ]
  :plugins [[lein-ritz "0.7.0"]
            [lein-clojars "0.9.1"]
            [lein-pprint "1.1.1"]]
  :repl-options  { 
                  :port 4001 
                  :init [(println "hello from ~/.lein/profiles.clj")]
		  :timeout 900000 ;; leiningen is sometimes a little slow to start
                  }
  :nrepl-middleware [inspector.middleware/wrap-inspect
                     ritz.nrepl.middleware.javadoc/wrap-javadoc
                     ritz.nrepl.middleware.apropos/wrap-apropos]}}

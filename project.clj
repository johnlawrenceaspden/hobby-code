(defproject hobby-code "0.0.0"
  :description ""
  :license ""
  :url ""
  :plugins [[cider/cider-nrepl "0.12.0"]
            [lein-ancient "0.6.8"] ;; lein ancient to find updated dependencies
            ] 
  :global-vars {*print-length* 100 *print-level* 100}
  
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.trace "0.7.5"]
                 [com.cemerick/pomegranate "0.3.1"]
;;                 [simple-plotter "0.1.2"]
                 ])

;; Examples of how to use pomegranate to load other libraries

;;(require 'cemerick.pomegranate)

;; (cemerick.pomegranate/add-dependencies
;; :coordinates [[net.sourceforge.parallelcolt/parallelcolt "0.10.0"]])

;; (cemerick.pomegranate/add-dependencies 
;;  :coordinates '[[hiccup "1.0.5"]]
;;  :repositories {"clojars" "http://clojars.org/repo" })

;; (cemerick.pomegranate/add-dependencies 
;;  :coordinates '[[org.clojure/test.check "0.9.0"]]
;;  :repositories (merge cemerick.pomegranate.aether/maven-central
;;                       {"clojars" "http://clojars.org/repo"}))

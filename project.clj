(defproject hobby-code "0.0.1"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [com.cemerick/pomegranate "0.0.13"]]

;; The magic words are:
;; (require 'cemerick.pomegranate)
;; (cemerick.pomegranate/add-dependencies 
;;  :coordinates '[[ring "1.1.7"]]
;;  :repositories {"clojars" "http://clojars.org/repo" } )

  :repl-options { 
                 :port 4001 
                 :init (println "hello from hobby-code/project.clj")
                 }
  :source-paths ["."]
  :min-lein-version "2.0.0"
)


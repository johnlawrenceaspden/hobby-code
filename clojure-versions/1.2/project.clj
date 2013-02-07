(defproject hobby-code "0.0.1"
  :dependencies [[org.clojure/clojure "1.2.0"]]
  :repl-options { 
                 :port 4002 
                 :init (println "hello from clojure 1.2")
                 }
  :source-paths ["."]
  :min-lein-version "2.0.0"
)


(defproject hobby-code "0.0.1"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :repl-options { 
                 :port 4001 
                 :init (println "hello from hobby-code/project.clj")
                 }
  :source-paths ["."]
  :min-lein-version "2.0.0"
)


(defproject ring-wiki "0.0.1"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [ring "1.1.6"]
                 [compojure "1.1.4"]
                 [enlive "1.0.1"]]
  :dev-dependencies [[lein-marginalia "0.7.1"]]
  :repl-options { 
                 :port 4001 
                 :init (println "hello from ring-wiki/project.clj")
                 }
  :source-paths ["."]
  :min-lein-version "2.0.0"
)


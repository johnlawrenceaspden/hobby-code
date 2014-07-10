(defproject hobby-code "0.0.1"
  :dependencies [
;                 [org.clojure/core.logic "0.8.1"]
;                 [net.mikera/core.matrix "0.22.0"]
;                 [org.clojars.achim/multiset "0.1.0-SNAPSHOT"]
;                 [simple-plotter "0.1.2"]
                 [cascalog "2.0.0"]
                 ]

  :profiles 
  {:dev 
   {:dependencies [[org.apache.hadoop/hadoop-core "1.1.2"]]}}
  :jvm-opts ["-Xms768m" "-Xmx768m"]



;;  Heap to 800M, Stack to 50M
;;  :jvm-opts ["-Xmx800M", "-Xss50M"]

;; Apparently the tiered compilation thing is leiningen throwing the JVM's optimizations overboard in order to reduce its own startup time.
;; This doesn't seem to make any difference either way
;; On the other hand, Dmitry pointed out that my java starts up in client mode
;; This used to work, but has now stopped doing, so lein repl starts up in client mode with optimizations turned off.
;;  :jvm-opts ^:replace ["-server"]

  :source-paths ["."]
)


(defproject hobby-code "0.0.1"
  :dependencies [
                 [org.clojure/core.logic "0.8.1"]
 ;                [org.clojars.achim/multiset "0.1.0-SNAPSHOT"]
                 ]

;;  Heap to 800M, Stack to 50M
;;  :jvm-opts ["-Xmx800M", "-Xss50M"]

;; Apparently the tiered compilation thing is leiningen throwing the JVM's optimizations overboard in order to reduce its own startup time.
;; This doesn't seem to make any difference either way
  :jvm-opts ^:replace []

  :source-paths ["."]
)


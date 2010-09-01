;; this runs with mvn clojure:run

(println "hello amazing uber-world")

(use '(incanter core charts))
(view (function-plot sin -4 4))

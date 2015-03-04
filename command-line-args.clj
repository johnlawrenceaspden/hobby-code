#^:shebang '[
exec java -cp "$HOME/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar" clojure.main "$0" "$@"
]
(prn *command-line-args*)

{:user 
 {:plugins [[lein-ritz "0.7.0"]
            [lein-clojars "0.9.1"]]
  :dependencies [[nrepl-inspect "0.3.0"]
                 [ritz/ritz-nrepl-middleware "0.7.0"]]
  :repl-options {:nrepl-middleware [inspector.middleware/wrap-inspect
                                    ritz.nrepl.middleware.javadoc/wrap-javadoc
                                    ritz.nrepl.middleware.apropos/wrap-apropos]}}}

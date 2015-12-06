(defproject hobby-code "0.0.0"
  :description ""
  :license ""
  :url ""
  :dependencies [[org.clojure/clojure "LATEST"]
                 [org.clojure/tools.trace "LATEST"]
                 [org.clojure/tools.nrepl "LATEST"]
                 [net.mikera/core.matrix "0.47.0"]
                 [net.mikera/vectorz-clj "0.37.0"]
                 [clatrix "0.5.0"]
                 [simple-plotter "LATEST"]]
  :plugins [[cider/cider-nrepl "0.10.0"]
            [lein-exec "0.3.5"]]
  :global-vars {*print-length* 100})


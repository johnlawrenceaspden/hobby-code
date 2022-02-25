(defproject hello "0.0.0"
  :dependencies [[nrepl/nrepl "0.8.3"]    ;; needed because default nrepl version conflicts with cider-nrepl
                 [org.clojure/clojure "1.10.3"]]
  :plugins [[cider/cider-nrepl "0.27.4" ]] ;; goes with cider 1.2.0, current emacs version
  :global-vars {*print-length* 150 *print-level* 100})


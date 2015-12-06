;Manually starting an nrepl server
(require 'cider.nrepl)
(require 'clojure.tools.nrepl.server)
(clojure.tools.nrepl.server/start-server :port 7888 :handler cider.nrepl/cider-nrepl-handler)

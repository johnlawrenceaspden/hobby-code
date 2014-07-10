(use 'cascalog.api)
(use 'cascalog.playground)
(require '[cascalog.logic.def :as def])

(def/defmapcatfn tokenise [line]
        (clojure.string/split line #"[\[\]\\\(\),.)\s]+"))

(?- (stdout)
    (<- [?word]
        (sentence :> ?line)
        (tokenise :< ?line :> ?word)))


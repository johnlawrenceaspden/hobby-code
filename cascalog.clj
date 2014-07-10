;; This fought with something in my user.clj 
;; solution is to move the hidden ~/.lein directory to olddotlein
;; so that the user.clj file doesn't get read in

(use 'cascalog.api)
(use 'cascalog.playground)
(require '[cascalog.logic.def :as def])

(def/defmapcatfn tokenise [line]
        (clojure.string/split line #"[\[\]\\\(\),.)\s]+"))

(?- (stdout)
    (<- [?word]
        (sentence :> ?line)
        (tokenise :< ?line :> ?word)))


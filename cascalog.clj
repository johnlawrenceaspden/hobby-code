;; Using Cascalog  [cascalog "2.0.0"]

;; This fought with something in my user.clj 
;; solution is to move the hidden ~/.lein directory to olddotlein
;; so that the user.clj file doesn't get read in

;; Ripped off from
;; http://cascalog.org/articles/getting_started.html

(use 'cascalog.api)
(use 'cascalog.playground)

sentence ;-> [["Four score and seven years ago our fathers brought forth on this continent a new nation"]["conceived in Liberty and dedicated to the proposition that all men are created equal"] ["Now we are engaged in a great civil war testing whether that nation or any nation so"] ["conceived and so dedicated can long endure We are met on a great battlefield of that war"] ["We have come to dedicate a portion of that field as a final resting place for those who"] ["here gave their lives that that nation might live It is altogether fitting and proper"] ["that we should do this"] ["But in a larger sense we can not dedicate  we can not consecrate  we can not hallow"] ["this ground The brave men living and dead who struggled here have consecrated it"] ["far above our poor power to add or detract The world will little note nor long remember"] ["what we say here but it can never forget what they did here It is for us the living rather"] ["to be dedicated here to the unfinished work which they who fought here have thus far so nobly"] ["advanced It is rather for us to be here dedicated to the great task remaining before us "] ["that from these honored dead we take increased devotion to that cause for which they gave"] ["the last full measure of devotion  that we here highly resolve that these dead shall"] ["not have died in vain  that this nation under God shall have a new birth of freedom"] ["and that government of the people by the people for the people shall not perish"] ["from the earth"]]

(count sentence) ; There are 18 lines in the Gettysburg address

;; Not sure why each string needs its own vector!
(first sentence) ;-> ["Four score and seven years ago our fathers brought forth on this continent a new nation"]
(ffirst sentence) ;-> "Four score and seven years ago our fathers brought forth on this continent a new nation"

;; Cascalog queries look like this
(?- (stdout) sentence)
;; This churns for a while as it spins up hadoop, then prints the contents of sentence
;; on the terminal (not in the repl, on the terminal where the repl was started)

;; We can say the same thing in a more complicated way
;; <- is the 'query creation operation'
(?- (stdout)
    (<- [?line]
        (sentence :> ?line)))



;; We can split the lines into words like this
(clojure.string/split 
 (ffirst sentence)
 ) ;-> ["Four" "score" "and" "seven" "years" "ago" "our" "fathers" "brought" "forth" "on" "this" "continent" "a" "new" "nation"]


;; And we can make that into a hadoop operation
(require '[cascalog.logic.def :as def])

(def/defmapcatfn tokenise [line]
        (clojure.string/split line #"[\[\]\\\(\),.)\s]+"))

(?- (stdout)
    (<- [?word]
        (sentence :> ?line)
        (tokenise :< ?line :> ?word)))

;; I'd be tempted to do it this way
(defn to-tokens [line]
  (clojure.string/split line #"[\[\]\\\(\),.)\s]+"))

(to-tokens (ffirst sentence)) ;-> ["Four" "score" "and" "seven" "years" "ago" "our" "fathers" "brought" "forth" "on" "this" "continent" "a" "new" "nation"]

(def/defmapcatfn to-tokens-mapcatfn [line]
        (to-tokens line ))

(?- (stdout)
    (<- [?word]
        (sentence :> ?line)
        (to-tokens-mapcatfn :< ?line :> ?word)))
;; Which makes me wonder what defmapcatfn is doing. I mean, why aren't all fns mapcatfns?
;; I think the :< and :> are pipelining operators like in unix












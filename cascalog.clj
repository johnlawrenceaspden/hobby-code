;; Using Cascalog  [cascalog "2.0.0"]

;; This fought with something in my user.clj 
;; solution is to move the hidden ~/.lein directory to olddotlein
;; so that the user.clj file doesn't get read in

;; I think what's happening is that hadoop is launching sub-clojures
;; which are going through leiningen somehow and so if the .lein file
;; is as usual, they re-use user.clj and something in my usual startup
;; code is clashing with the hadoop code.

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
 #"[\[\]\\\(\),.)\s]+") ;-> ["Four" "score" "and" "seven" "years" "ago" "our" "fathers" "brought" "forth" "on" "this" "continent" "a" "new" "nation"]


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

;; Data Tuples

;; look at some vars from the playground
person ;-> [["alice"] ["bob"] ["chris"] ["david"] ["emily"] ["george"] ["gary"] ["harold"] ["kumar"] ["luanne"]]
age ;-> [["alice" 28] ["bob" 33] ["chris" 40] ["david" 25] ["emily" 25] ["george" 31] ["gary" 28] ["kumar" 27] ["luanne" 36]]

(?- (stdout)
    (<- [?doom]
        (age ?foo ?doom)))
;; 28
;; 33
;;... appear on console

(?- (stdout)
    (<- [?foo]
        (age ?foo ?doom)))
;; alice
;; bob
;;... appear

;; Recall from above
(def/defmapcatfn tokenise [line]
        (clojure.string/split line #"[\[\]\\\(\),.)\s]+"))

(tokenise sentence) ; urk

;; This appears to be a sane thing to do though
(tokenise (ffirst sentence)) ; ["Four" "score" "and" "seven" "years" "ago" "our" "fathers" "brought" "forth" "on" "this" "continent" "a" "new" "nation"]

;; I'm a bit worried that
(?- (stdout) 
    (<- [?word]
        ((first sentence) :> ?line)
        (tokenise :< ?line :> ?word)))

;; both these appear to work as expected
(?- (stdout) 
    (<- [?word]
        (sentence :> ?line)
        (tokenise :< ?line :> ?word)))


;; Now, an incomprehensible word count example:
(require '[cascalog.logic.ops :as c])

(?- (stdout)
    (<- [?word ?count]
        (sentence :> ?line)
        (tokenise :< ?line :> ?word)
        (c/count :> ?count)))

;; ....
;; who	        3
;; will	        1
;; work	        1
;; world	1
;; years	1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here I'm stealing from http://blog.factual.com/clojure-on-hadoop-a-new-hope

(def cities
  ["New York, NY"
   "Chicago, IL"
   "Los Angeles, CA"
   "San Francisco, CA"
   "Seattle, WA"])

; csv parser from 
; [clojure-csv/clojure-csv "2.0.1"]
(require '[clojure-csv.core :as csv] )

(first cities) ; "New York, NY"
(csv/parse-csv (first cities)) ;-> (["New York" " NY"])

(defn cities-parser [line]
  (map #(.trim %) (first (csv/parse-csv line))))

(cities-parser (first cities)) ; ("New York" "NY")

(use 'cascalog.api)

;; run cities-parser on every line of our csv file
(?<-
 (stdout)
 [?city ?state]
 (cities ?line)
 (cities-parser ?line :> ?city ?state))


;; [org.clojure/data.json "0.2.5"]
(require '[clojure.data.json :as json])

(def buildings [" {\"name\":\"Chrysler Building\",\"city\":\"New York\"}",
                 "{\"name\":\"Empire State Building\",\"city\":\"New York\"}"])
                 {\"name\":\"John Hancock Center\",\"city\":\"Chicago\"}
                 {\"name\":\"Walt Disney Concert Hall\",\"city\":\"Los Angeles\"}
                 {\"name\":\"Transamerica Pyramid\",\"city\":\"San Francisco\"}
                 {\"name\":\"Space Needle\",\"city\":\"Seattle\"}")




(json/read-str buildings) ; {"name" "Chrysler Building", "city" "New York"}

(defn buildings-parser[line] (map (json/read-str line) ["name" "city"]))

(buildings-parser buildings)

(?<- (stdout) [?name ?city]
     (buildings ?line)
     (buildings-parser ?line :> ?name ?city))

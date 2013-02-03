;; Regular Expressions in Clojure

;; Every time I want to use a regex in clojure I find myself having to
;; learn how to do it again.

;; for some reason the functions just won't stick in my mind.

;; There are six functions starting with re- in clojure.core:

(def d (with-out-str 
         (doc re-seq)
         (doc re-pattern)
         (doc re-find)
         (doc re-groups)
         (doc re-matcher)
         (doc re-matches)))

;; This is probably the function that you want:
(re-seq   #"f.nd" d) ;-> ("find" "find" "find" "find" "find")

;; re-pattern is for making regular expressions out of strings
(re-pattern "f.nd") ;-> #"f.nd"

;; But normally we'd just use the syntactic sugar #"f.nd" directly

;; The return type is dependent on the regex in a nasty way:

(re-seq   #"f..d" d)   ;-> ("find" "find" "find" "find" "find")
(re-seq   #"f(.)nd" d) ;-> (["find" "i"] ["find" "i"] ["find" "i"] ["find" "i"] ["find" "i"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simple regexes

;; Most characters will match themselves

(def greek "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu xi omicron pi rho sigma tau upsilon phi chi psi omega")

;; There are lots of "a"s in greek
(re-seq #"a" greek) ;-> ("a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a")   

;; But not so many "b"s
(re-seq #"b" greek) ;-> ("b" "b")

;; A . will match anything

(take 20 (re-seq #"." greek)) ; -> ("a" "l" "p" "h" "a" " " "b" "e" "t" "a" " " "g" "a" "m" "m" "a" " " "d" "e" "l")

;; * means 'any number of the previous thing'
(re-seq #"ga*m*a" greek)

;; So .* means any number of any thing
(re-seq #".*" greek) ; -> ("alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu xi omicron pi rho sigma tau upsilon phi chi psi omega" "")

;; \b is a word boundary, \w is non-whitespace, so:
(re-seq #"\b\w*\b" greek) ; -> ("alpha" "" "beta" "" "gamma" "" "delta" "" "epsilon" "" "zeta" "" "eta" "" "theta" "" "iota" "" "kappa" "" "lambda" "" "mu" "" "nu" "" "xi" "" "omicron" "" "pi" "" "rho" "" "sigma" "" "tau" "" "upsilon" "" "phi" "" "chi" "" "psi" "" "omega" "")

;; but we found too much there. Instead of * meaning any number, we can say:

(re-seq #"\b\w{0}\b" greek) ; ("" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")
(re-seq #"\b\w{1}\b" greek) ; nil
(re-seq #"\b\w{2}\b" greek) ; ("mu" "nu" "xi" "pi")
(re-seq #"\b\w{3}\b" greek) ; ("eta" "rho" "tau" "phi" "chi" "psi")
(re-seq #"\b\w{4}\b" greek) ; ("beta" "zeta" "iota")


;; Ranges work too
(re-seq #"\b\w{1,4}\b" greek) ; ("beta" "zeta" "eta" "iota" "mu" "nu" "xi" "pi" "rho" "tau" "phi" "chi" "psi")
(re-seq #"\b\w{5}\b" greek) ; ("alpha" "gamma" "delta" "theta" "kappa" "sigma" "omega")
(re-seq #"\b\w{6,}\b" greek) ; ("epsilon" "lambda" "omicron" "upsilon")

;; + is a shorthand for {1,} in the same way as * is one for {0,}
(re-seq #"\b\w+\b" greek) ; ("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")

;; we can use character ranges:
(re-seq #"\b[a-o]+\b" greek) ; ("gamma" "lambda" "chi" "omega")

;; or exclude certain characters:
(re-seq #"\b[^ ]+\b" greek) ; ("gamma" "lambda" "chi" "omega")





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Here are some classic example regexes (ripped off from
;; http://myregexp.com/examples.html), because I can never remember
;; how the more complicated cases work:

;; Behold the non-capturing group (?:...), the alternation | , the
;; greedy 0 or 1 ?, the greedy 3 {3} and the word boundary \b:

(re-seq #"\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b"
        "Here's an ip address:111.123.0.127, and here is another 136.54.23.108 
but this is not one 1.1.1.257 and neither is this: 243.1.231 or this 1.1.1.1.1 but is this?255.000.1.255")

;; Here we demonstrate the both the 'repeated capturing group' problem for MAC addresses
(re-seq #"^([0-9a-fA-F][0-9a-fA-F]:){5}([0-9a-fA-F][0-9a-fA-F])$"
        "AA:0a:be:23:01:02") ;-> (["AA:0a:be:23:01:02" "01:" "02"])

;; And a use case for re-pattern, together with the (?m) multiline flag
(re-seq (re-pattern (str "(?m)^" 
                         (clojure.string/join ":" (repeat 6 "([0-9a-fA-F][0-9a-fA-F])"))
                         "$"))
        "AA:0a:be:23:01:02\nAA:0a:be:123:01:02\nAG:0a:be:23:01:02\n00:01:02:03:04:55\nAA:0a:be:23:0:02:AA\n:0a:be:23:01:02\n")
;-> (["AA:0a:be:23:01:02" "AA" "0a" "be" "23" "01" "02"] 
;->  ["00:01:02:03:04:55" "00" "01" "02" "03" "04" "55"])


(re-seq #"\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b"
        "Here's an ip address:111.123.0.127, and here is another 136.54.23.108 
but this is not one 1.1.1.257 and neither is this: 243.1.231 or this 1.1.1.1.1 but is this?255.000.1.255")


(re-seq #"\b([a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,6}\b"
"Amongst the worlds domain names are such as www.google.com, ssh.aspden.com,
 aspden.com, aspden.co.uk, 123.com, ssh.123.com and .com")








;; The other four functions in clojure seem to be to part of the implementation
;; of re-seq, and directly manipulate highly stateful
;; java.util.regex.Matcher objects

;; re-find, re-groups, re-matcher, and re-matches seem to be only
;; useful as part of re-seq, and I wonder if they should be
;; deprecated, or moved to a namespace that needs to be specifically
;; imported.

;; Although if you just want the first match then :
(re-find   #"f.nd" d)
;; seems to be an acceptable alternative to:
(first (re-seq #"f.nd" d))


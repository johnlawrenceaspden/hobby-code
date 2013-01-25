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


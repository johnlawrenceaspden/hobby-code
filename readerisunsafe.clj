;; Clojure's Reader is Unsafe

;; dependencies:
;; [org.clojure/clojure "1.4.0"]
;; [org.clojure/tools.reader "0.7.0"]


;; By a weird coincidence this week I wanted to read a file of data
;; coming from a web app, and I was about to use Clojure's reader to
;; do it. I remembered that there was some variable that I needed to
;; bind to stop it executing arbitrary code, so I went googling.

;; And found that there's been a fair bit of debate on just this topic recently.

;; Essentially, Clojure's reader is the thing that turns strings into
;; data structures when you're reading in a program or typing at the
;; REPL. And you have it available programmatically.

(read-string "(+ 1 2)") ; (+ 1 2)

;; Not so impressive, you say?

;; Consider:

(first "(+ 1 2)") ; \(
(second "(+ 1 2)") ; \+
(count "(+ 1 2)") ; 7
(type "(+ 1 2)") ; java.lang.String
(type (first "(+ 1 2)")) ; java.lang.Character

;; "(+ 1 2)" is a String, a flat data structure of seven Characters

(first (read-string "(+ 1 2)")) ; + 
(second (read-string "(+ 1 2)")) ; 1
(count (read-string "(+ 1 2)")) ; 3 
(type  (read-string "(+ 1 2)")) ; clojure.lang.PersistentList
(type (first (read-string "(+ 1 2)"))) ; clojure.lang.Symbol

;; Clearly some sort of sea-change has occurred.

;; Even better, the value of a string is a similar string
(eval "(+ 1 2)") ; "(+ 1 2)"
(identical? (eval "(+ 1 2)") "(+ 1 2)") ; true
;; But the thing that read-string makes is a program which can run
(eval (read-string "(+ 1 2)")) ; 3
;; Another way to make such a program is:
(eval (list (quote +) (quote 1) (quote 2))) ; 3

;; In my innocence, I was going to use this magic to read some data out of a file that had been produced
;; by a web server.

(def file "{:username leethaxor :score 5 :alignment :chaotic-evil}")

(:username (read-string file)) ;-> leethaxor


;; IT TURNS OUT THAT YOU SHOULD NEVER DO THIS, EVEN THOUGH THE ABILITY
;; TO DO THIS IS ONE OF THE THINGS THAT IS NICE ABOUT LISPS.

;; There are various reasons why:

;; Firstly, note that programs running on a computer can affect that computer

;; If you happen to have a file called precious.txt in the directory
;; where clojure is running, then you are strongly advised not to
;; execute any of the following code. Especially if it contains your
;; first novel and you haven't got round to backing up yet this year.

(clojure.java.shell/sh "touch" "precious.txt") ;-> {:exit 0, :out "", :err ""}
(clojure.java.shell/sh "cat" "precious.txt") ;-> {:exit 0, :out "", :err ""}


;; Secondly, note that there is special syntax to cause things to happen while
;; reading: 

(read-string "    #=(clojure.java.shell/sh \"rm\" \"precious.txt\")    ") ;-> {:exit 0, :out "", :err ""}

;; That did not get turned into a data structure. That got executed. precious.txt is gone.

(clojure.java.shell/sh "cat" "precious.txt") ;-> {:exit 1, :out "", :err "cat: precious.txt: No such file or directory\n"}

;; Luckily we can rebuild it:
(clojure.java.shell/sh "touch" "precious.txt") ;-> {:exit 0, :out "", :err ""}

(clojure.java.shell/sh "cat" "precious.txt") ;-> {:exit 0, :out "", :err ""}

;; And thirdly note that not everyone on the internet is on your side:
(def file-of-evil " {:username #=(clojure.java.shell/sh \"rm\" \"precious.txt\") :score 5 :alignment :chaotic-evil}")

(read-string file-of-evil) ;-> {:username {:exit 0, :out "", :err ""}, :score 5, :alignment :chaotic-evil}

;; This is a thing that you're supposed to guard against when reading data structures.

(clojure.java.shell/sh "touch" "precious.txt") ;-> {:exit 0, :out "", :err ""}

;; It is traditional to do that like this:

(binding [*read-eval* false] (read-string "(+ 1 2)")) ;-> (+ 1 2)
(binding [*read-eval* false] (read-string file-of-evil))
; -> RuntimeException EvalReader not allowed when *read-eval* is false.  clojure.lang.Util.runtimeException (Util.java:170)

;; And in my innocence, I was about to actually do this, which would have been a mistake:

(def string-of-evil "#java.io.FileWriter[\"precious.txt\"]")

(spit "precious.txt" "precious content") ;-> nil
(slurp "precious.txt") ;-> "precious content"
(binding [*read-eval* false] (clojure.core/read-string string-of-evil)) ;-> #<FileWriter java.io.FileWriter@17b2712>
(slurp "precious.txt") ;-> ""

;; bugger

;; It turns out that even with *read-eval* bound to false, the Clojure
;; reader can be persuaded to execute arbitrary java constructors, and
;; some of those have side-effects.

;; I believe that that particular loophole is going to be fixed in the
;; next version of clojure, but the general thinking seems to be that
;; the reader is just intrinsically an unsafe thing and that you
;; shouldn't use it to read strings which you aren't sure are friendly.

;; Luckily, there is a library function that does what I thought the
;; reader would do, and that is apparently Absolutely Guaranteed by
;; Act of Parliament not to do bad things:

(require 'clojure.tools.reader.edn)

(spit "precious.txt" "precious content") ;-> nil
(slurp "precious.txt") ;-> "precious content"
(clojure.tools.reader.edn/read-string string-of-evil)
;; -> ExceptionInfo No reader function for tag java.io.FileWriter  clojure.core/ex-info (core.clj:4227)
(clojure.tools.reader.edn/read-string file-of-evil)
;; -> ExceptionInfo No reader function for tag =  clojure.core/ex-info (core.clj:4227)

;; But which is still usable for the purposes of good:
(clojure.tools.reader.edn/read-string file) ;-> {:username leethaxor, :score 5, :alignment :chaotic-evil}
(clojure.tools.reader.edn/read-string "(+ 1 2)") ;-> (+ 1 2)
(eval (clojure.tools.reader.edn/read-string "(+ 1 2)")) ; 3

(slurp "precious.txt") ;-> "precious content"

;; So that's Super News!

;; We've got a function, read-string, which can turn strings into data structures while doing no harm whatsoever,
;; and another function, read-string, which can turn strings into data structures while potentially causing arbitrary side effects depending on the content of the strings.

;; Clearly nothing could possibly go wrong with this arrangement,
;; especially since read-string will be fixed in clojure 1.5 so that
;; binding *read-eval* to false around it will make it safe, as far as
;; anyone knows, even though the advice is still never to use it to
;; read untrusted strings, for some reason.




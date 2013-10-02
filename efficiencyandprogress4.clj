;; Efficiency and Progress IV: Avoiding Leiningen

;; It turns out that there are many relevant variables when attempting
;; to speed up Clojure.

;; Java can start in 'client mode' or 'server mode'. The difference
;; seems to be that 'server mode' is faster, and speeds up as you run
;; it.  

;; It's also, rather amazingly, true that leiningen turns off the
;; default jvm runtime optimizations!

;; You're supposed to be able to control this by adding

  :jvm-opts ^:replace ["-server"]

;; to project.clj, and initially this did seem to work for me.

;; However now it's stopped working, and things that were fast have
;; become slow.

;; You can get the classpath that leiningen would use like this:

LEIN_CLASSPATH=`lein classpath`

;; And then start a repl with:

rlwrap java -server -classpath $LEIN_CLASSPATH clojure.main 

;; Apart from the initial run of leiningen to work out the classpath,
;; which you only have to do occasionally, this is a much quicker way
;; to start a repl, and rlwrap provides a command-line environment
;; that works like the bash shell and which I find very nice.

;; Of course, you'll want to make a version that will talk to emacs
;; via nrepl, and providing that the nrepl jar is on the classpath,
;; this will do the trick:

rlwrap java -server -classpath $LEIN_CLASSPATH clojure.main -e "( do (require 'clojure.tools.nrepl.server) (clojure.tools.nrepl.server/start-server :bind \"127.0.0.1\" :port 4001))" -r

;; In fact, if you have clojure-1.5.1 and nrepl 0.2.3 in your maven repository, then you can create a minimal classpath like this:
CLP=$HOME/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar:$HOME/.m2/repository/org/clojure/tools.nrepl/0.2.3/tools.nrepl-0.2.3.jar
;; And then run the clojure/repl/nrepl process like this:
rlwrap java -server -classpath $CLP clojure.main -e "( do (require 'clojure.tools.nrepl.server) (clojure.tools.nrepl.server/start-server :bind \"127.0.0.1\" :port 4001))" -r

;; And there are many variations on this theme.



;; I often find that the default behaviour of exceptions in emacs/slime is a bit
;; annoying.

;; Up pops up a debugger window, but that moves the focus away from where
;; you're working, and the new window then needs dismissing, and has often
;; buggered up the size of other windows, etc...

;; Also when playing with experimental clojure versions, the behaviour is a bit
;; random, and often the expression just gets swallowed, with no clue as to what
;; it was:

;; Anyway I wrote this little macro, which turns an exception into a return value:
(defmacro tryc[ & e]
  `(try ~@e
       (catch Exception a# a#)))


(tryc (java.io.File ".")) ;; #<ClassCastException java.lang.ClassCastException: java.lang.Class cannot be cast to clojure.lang.IFn>

;; Obviously you should only use this at the top level!

;; And I find I'm using it all the time when writing code in emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; However sometimes, of course, you actually want to look at the vast stack
;; trace that always accompanies these things.

;; Even then I still don't find the debugger that helpful, because without local
;; variables for each stack frame it's not as useful as it could be, although
;; the ability to jump around the code with M-p and M-n is quite handy sometimes.

;; And generally, the error is in my code, rather than in clojure.core or in swank
;; or in any of the manifold .java files which make up the language.

;; So I wrote another macro which strips all that information and just leaves
;; the bits of stack trace that you might be interested in:

(defmacro guru [& e]
  `(try ~@e
     (catch Exception a#
       (let [message# (.getMessage a#) 
             st# (seq (.getStackTrace a#))
             unusual# (filter #(let [fn# (.getFileName %)]
                                (and (not (re-matches #".*\.java" fn#))
                                     (not (#{"basic.clj" "core.clj"} fn# ))))  st#)]
         ;; print formatted
         (println (class a#) message# "\n--")
         (pp/pprint (map #(vector (.getFileName %)(.getLineNumber %)(.getClassName %)) unusual#))
         (println "--\n" (interpret a#))
         ;; return value
         (list (class a#) message#
               (map #(vector (.getFileName %)(.getLineNumber %)(.getClassName %)) unusual#)
               (interpret a#)))))))

;; And also I find some of the errors rather mystifying, so I decided to keep a list of hints towards common causes 

(defn interpret [e]
  (cond (and
         (= (class e) java.lang.ClassCastException)
         (= (.getMessage e) "java.lang.Class cannot be cast to clojure.lang.IFn"))
        "Possibly you forgot a dot.\n(java.Class arg) rather than\n(java.Class. arg)\n can cause this.",
        ;; but I've only got one so far
        :else
        "Confucius he largely mystified by this.")) ; so my guru is not as helpful as he could be

;; here's a function that doesn't work

(defn cause-error[]
  (java.io.File "."))

;; The guru ignores code that works
(guru (java.io.File. ".")) ; #<File .>

;; But if he sees an exception bubbling up:
(guru (cause-error))

;; He tries his best printing to stdout
;; java.lang.ClassCastException java.lang.Class cannot be cast to clojure.lang.IFn 
;; --
;; (["clojure-errs-dict.clj" 62 "user$cause_error"]  <-- the error was totally on line 62
;;  ["NO_SOURCE_FILE" 1 "user$eval12716"]            <-- but here is stuff to do with evaluation with C-x-e
;;  ["NO_SOURCE_FILE" -1 "user$eval12714"])
;; --
;;  Possibly you forgot a dot.                       <-- and in this case, he has nailed the cause.
;; (java.Class arg) rather than
;; (java.Class. arg)
;;  can cause this.

;; And returns the following value which you can see at the bottom of the screen
'(java.lang.ClassCastException "java.lang.Class cannot be cast to clojure.lang.IFn" (["clojure-errs-dict.clj" 62 "user$cause_error"] ["NO_SOURCE_FILE" 1 "user$eval12716"] ["NO_SOURCE_FILE" -1 "user$eval12714"]) "Possibly you forgot a dot.\n(java.Class arg) rather than\n(java.Class. arg)\n can cause this.")


;; If he doesn't understand, then he admits his ignorance

(print (guru (/ 1 0)))
;; java.lang.ArithmeticException Divide by zero 
;; --
;; (["NO_SOURCE_FILE" 1 "user$eval12695$fn__12696"]  <-- again because I'm evaluating in the emacs buffer there's not much here
;;  ["NO_SOURCE_FILE" 1 "user$eval12695"]            <-- if you compile the file with C-c C-k then the traces are more use
;;  ["NO_SOURCE_FILE" -1 "user$eval12693"])
;; --
;;  Confucius he largely mystified by this.


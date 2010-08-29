;; It's been mentioned that my approach of posting syntax-highlighted posts is less than optimally readable.

;; I wonder if there's a way to split the files up into pieces which can be individually syntax-highlighted

;; We can use this command on the command line to highlight a clojure file
;; $ emacs --eval "(htmlize-file \"/home/john/file.clj\" ) (kill-emacs)"

;; And since the latest clojure and clojure-contrib are in my local maven repository, the command to execute this file is:
;; java -cp "/home/john/.m2/repository/org/clojure/clojure/1.2.0/clojure-1.2.0.jar:/home/john/.m2/repository/org/clojure/clojure-contrib/1.2.0/clojure-contrib-1.2.0.jar" clojure.main blogpost.clj 


(require 'clojure.java.shell)
(let [[ {:keys [exit output error]} ](clojure.java.shell/sh "emacsclient" "--eval" "(htmlize-file \"/home/john/hobby-code/blogpost.clj\") (kill-emacs)" )]
  (println exit)
  (println output)
  (println error))
#!/usr/bin/env clojure

;; It looks like clojure has become a full citizen of Debian

;; In emacs, a syntax highlighted program can be converted to html with htmlize-buffer, and then if
;; you top and tail that and copy it to blogger it comes up nicely. And this seems to still be
;; working after all this time

;; So here is my first published clojure program for a while...

;; If you're using Debian 11, you can run it by installing clojure with
;; $ sudo apt install clojure

;; saving this file as test.clj

;; and then in a terminal window making the file executable
;; $ chmod +x test.clj
;; and then running it
;; $ ./test.clj

(println "hello")

(defn factorial [n]
  (if (< n 1) 1
      (* n (factorial (- n 1)))))

(println '(factorial 10) (factorial 10))

;; If all is working then you should see something like
;; $ ./test.clj
;; hello
;; (factorial 10) 3628800

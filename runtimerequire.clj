;; How to pull in a library at runtime

;; dependencies [[org.clojure/clojure "1.4.0"]
;;               [com.cemerick/pomegranate "0.0.13"]]


;; When working at the REPL, it's often a pain to stop clojure, add a library to your dependencies, and restart.

;; Behold the solution, due to Chas Emerick (on whom blessings)

(require 'cemerick.pomegranate)

;; We don't, I think, even need to download the library. Pomegranate will go off and get it if it needs to:

(cemerick.pomegranate/add-dependencies 
 :coordinates '[[ring "1.1.7"]]
 :repositories {"clojars" "http://clojars.org/repo" } )

;; It takes a while, but once it's done :

(require 'ring.adapter.jetty)

(defonce server (ring.adapter.jetty/run-jetty (fn[x]{:body"<h1>yo</h1>"}) {:port 8080 :join? false}))

;; And it works!

;; I am told that there are reasons not to use this in production
;; code, only at the REPL, which is a terrible shame, since otherwise
;; programs could specify their own dependencies and versions rather
;; than having to exile them to a separate file.

;; Still, this is a huge time-saver, and takes a lot of the pain out of trying a new library.
(ns hello)

(use 'compojure)

;; Router                                                                          
(defroutes my-app
  (GET "/" (html [:h1 "Hello World!"] "working!"))
  (ANY "*" (page-not-found)))

(defn hello []
  (println "hello!")
  (run-server {:port 8080} "/*" (servlet my-app)))

;(require 'hello)
;(hello/hello)

(use 'clojure.test)

(deftest the-test
  (is true))

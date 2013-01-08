;;  necessary dependencies 
;; [[org.clojure/clojure "1.4.0"]
;;  [ring/ring "1.1.6"]]
;; -------------

;; A ring application is a function which takes a request map, and
;; returns a response map

;; Our first response map will have the HTTP status code 200, OK, a
;; content-type header that tells the browser that it's getting html
;; and a traditional body text.

(defn app [request]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body "Hello World"})


;; Having got a ring application, we need to start a webserver to hand the pages out
;; We'll use jetty (via ring)
(require 'ring.adapter.jetty)

;; And we'd like those pages served on port 8080 

;; Three things to note here:
;;
;; :join? false means that the evaluating thread won't wait for the
;; server to finish (so that the repl doesn't seem to hang).
;;
;; referring to the application function via #' means that ring sees
;; the variable user/app rather than the function (fn[x]{:status 200})
;; which that variable evaluates to. And that means that if we
;; reevaluate the definition, the behaviour the browser sees will
;; change.
;;
;; finally defonce means that if we reload this file, or re-evaluate
;; this line, nothing will happen. That prevents us from accidentally
;; creating multiple copies of the jetty server.

(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))

;; So, go and look at http://localhost:8080 in your favourite browser.



;; Now let's check that redefining the handler causes a change in the running webapp
(defn app [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<h1>Hello World</h1>"})

;; Refresh your browser to see the change.

;; I like to leave the web browser of the gods:
;; $ watch -d -n 1 curl -sv http://localhost:8080/ 
;; running in a terminal somewhere.


;; Now, let's look at the information that is going in and out of our application
(require 'clojure.pprint)

;; First we'll delegate the actual functionality of our app to a handler

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<h1>Hello World</h1>"})

;; And then we'll define what's called a middleware

(defn app [request]
  (println "-------------------------------")
  (println "Incoming Request:")
  (clojure.pprint/pprint request)
  (let [response (handler request)]
    (println "Outgoing Response Map:")
    (clojure.pprint/pprint response)
    (println "-------------------------------")
    response))

;; And finally let's demonstrate that we can stop and restart our server

(.stop server)

(.start server)
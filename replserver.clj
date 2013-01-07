;; If you type this in at the REPL, you can stop and start the server, and it will update too


(defn app [req]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body "<h2>Hello from Clojure!\n</h2>"})

(use 'ring.adapter.jetty)

(def server (run-jetty #'app {:port 8080 :join? false}))


;; The web browser of the gods:
;; $ watch -d -n 1 curl -sv http://localhost:8080/ 



;; to stop
(.stop server)

;; to start
(.start server)

;; and you can just redefine app to get new behaviour. I think this is
;; because of the #' on app when passed to jetty.

;; for instance
(defn app [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<h2>Hello from Clojure!\n</h2>"})

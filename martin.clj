(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<h1>Hello World</h1>"})

(require 'ring.adapter.jetty)

(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))

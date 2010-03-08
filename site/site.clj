; Indicate which libraries we need, and the
; shorter names by which they will be called.
(ns site
     (:require [compojure.http.servlet :as servlet])
     (:require [compojure.http.routes :as routes])
     (:require [compojure.server.jetty :as jetty]))

; Respond to any request by saying "Hello."
(servlet/defservlet hello-servlet
     "A hello world web application"
     (routes/ANY "/*" "Hello."))

; This server will run on port 80 and use
; the hello-servlet that is defined above.
(jetty/defserver hello-server
     {:port 80}
     "/*" hello-servlet)

; Command to start the server
(jetty/start hello-server)


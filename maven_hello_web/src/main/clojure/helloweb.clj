(ns helloweb
  (:use compojure clojure.test))

(with-test

    (defn yo[s] (html [:head [:title "hello world"]][:body "yo"]))

  (is (=(yo 'dummy) "<head><title>hello world</title></head><body>yo</body>" )))

(defroutes my-app (GET "/" yo))

(defn start-web-server []
  (run-server {:port 8080} "/*" (servlet my-app)))
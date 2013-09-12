;; I was wondering how to show graphs in a web application

;; First use pomegranate to add the ring library to your classpath
(require 'cemerick.pomegranate)
(cemerick.pomegranate/add-dependencies   :coordinates '[[ring "1.2.0"]] :repositories {"clojars" "http://clojars.org/repo" } )

;; And pull in jetty
(require 'ring.adapter.jetty)

;; Create a minimal app
(defn app [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "Hello World"})

;; And serve it
(defonce server (ring.adapter.jetty/run-jetty #'app {:port 8080 :join? false}))

;; http://localhost:8080 should get you the traditional greeting

;; Now let's have a minimal svg drawing
(defn app [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<svg width=\"100%\" height=\"100%\" viewBox=\"0 0 1000 1000\">"
              "<line stroke=\"#000000\" x1=\"0\" y1=\"0\" x2=\"1000\" y2=\"1000\"/>"
              "<line stroke=\"#000000\" x1=\"1000\" y1=\"0\" x2=\"0\" y2=\"1000\"/>"
              "<rect x=\"400\" y=\"400\" width=\"200\" height=\"200\" style=\"fill:red;\" />"
              "</svg>")})

;; Note how the cross and square scale politely to fit as large a square
;; as the browser window can give.  The viewBox tells us that our
;; drawing coordinates are 1000x1000, and the width and height tell us
;; how much of the available space the drawing should take up.

;; We can make all that a bit easier to type:

(defn svg [{:keys [width height x0 y0 x1 y1] :or {width "100%" height "100%" x0 0 y0 0 x1 1000 y1 1000}} & contents]
  (str "<svg width=" width " height=" height " viewBox=\"" x0 " " y0 " " x1 " " y1 "\">" 
       (apply str contents)
       "</svg>"))

(defn line
  ([x1 y1 x2 y2 {:keys [stroke] :or {stroke "#000000"}}]
     (str  "<line stroke=" stroke " x1=" x1 " y1=" y1 " x2=" x2 " y2=" y2 " />")))

(defn rect 
  ([x y width height {:keys [style] :or {style "\"\""}}] (str "<rect x=" x " y=" y " width=" width " height=" height " style=" style " />")))

(declare app-body)
  
(defn app [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (app-body)})

;; This should produce similar results
(defn app-body []
  (svg {} 
       (line 0 0 1000 1000 {})
       (line 1000 0 0 1000 {})
       (rect 400 400 200 200 {:style "fill:red"})))

;; A histogram might look like this:
(defn app-body []
  (svg {}
         (rect 0   (- 1000 400) 100 400 {})
         (rect 100 (- 1000 300) 100 300 {:style "fill:red"})
         (rect 200 (- 1000 200) 100 200 {:style "fill:blue"})
         ))


;; Here's a random wiggly line:
(defn app-body []
  (apply svg {}
         (for [[[x1 y1][x2 y2]]
               (partition 2 1 (for [i (range 11)][(* 100 i) (rand-int 1000)]))]
           (line x1 y1 x2 y2 {}))))


;; And here's another histogram:
(defn app-body []
  (apply svg {}
         (map #(rect %1 (- 1000 %2) 100 %2 {:style "fill:grey"}) (iterate #(+ % 100) 0) [400 300 200 900 100 700 200 800])))






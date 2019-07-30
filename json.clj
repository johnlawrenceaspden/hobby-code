(require 'cemerick.pomegranate)


(cemerick.pomegranate/add-dependencies 
 :coordinates '[[org.clojure/data.json "0.2.6"]]
 :repositories (merge cemerick.pomegranate.aether/maven-central
                      {"clojars" "http://clojars.org/repo"}))


(require ['clojure.data.json :as 'json])

(json/write-str {:a 1 :b 2}) ; "{\"a\":1,\"b\":2}"

(json/read-str "{\"a\":1,\"b\":2}") ; {"a" 1, "b" 2}


(def in (json/read-str(slurp "/home/john/ankiexport/Latin__Lingua_Latina/Latin__Lingua_Latina.json")))

(spit "/home/john/ankiexport/Latin__Lingua_Latina/Latin__Lingua_Latina.json" (json/write-str in))

(spit "/home/john/ankiexport/Latin__Lingua_Latina/Latin__Lingua_Latina.json" (with-out-str (json/pprint in)))

(count (in "notes"))

(def list (for [n (in "notes") :when (not= ((n "fields")6) "")] [((n "fields")0) ((n "fields")6)]))

(first list) ; ["vocābulum -ī n" ""]
(nth list 1) ; ["ūnus -a -um" "[sound:ankidroid_audiorec1900652919.mp3]"]

(first (clojure.string/split (first (nth list 1)) #" ")) ; "ūnus"

( (clojure.string/split (second (nth list 1)) #"[\[\]:]") 2) ; "ankidroid_audiorec1900652919.mp3"


(for [[a,b] list]
  [(first (clojure.string/split a #"[, ]"))
   (nth (clojure.string/split b #"[\[\]:]") 2)])

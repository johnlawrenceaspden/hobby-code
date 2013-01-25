; using [enlive "1.0.1"]

(require '[net.cgrand.enlive-html :as html])

(def url (html/html-resource (java.net.URL. "http://www.aspden.com")))

(def realurl (((first (html/select url [:frame])) :attrs) :src))

(def realpage (html/html-resource (java.net.URL. realurl)))

(html/select realpage [:a])


(use 'clojure.pprint)

(pprint 
 (map 
  (fn[x] ((juxt :href :name)(x :attrs) ))
  (html/select realpage [:a])))



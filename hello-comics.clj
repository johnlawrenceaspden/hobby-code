;;from the clojure tutorial at:
;;http://gnuvince.wordpress.com/2008/10/31/fetching-web-comics-with-clojure-part-1/

(def *comics*
     [{:name "Penny Arcade"
       :url "http://www.penny-arcade.com/comic/"
       :regex #"images/\d{4}/.+?(?:png|gif|jpg)"
       :prefix "http://www.penny-arcade.com/"
       }
      {:name "We The Robots"
       :url "http://www.wetherobots.com/"
       :regex #"comics/.+?[.](?:jpg|png|gif)"
       }
      {:name "Xkcd"
       :url "http://www.xkcd.com"
       :regex #"comics"
       :type :tooltip-comic
       }])

(import '(java.net URL)
        '(java.lang StringBuilder)
        '(java.io BufferedReader InputStreamReader))

(defn fetch-url
  "Return the web page as a string."
  [address]
  (let [url (URL. address)]
    (with-open [stream (. url (openStream))]
      (let [buf (BufferedReader. (InputStreamReader. stream))]
        (apply str (line-seq buf))))))

(defn image-url
  "Return the absolute URL of the image of a comic.
  If the comic has a prefix, prepend it to the URL,
  otherwise use the :url value."
  [comic]
  (let [src (fetch-url (:url comic))
        image (re-find (:regex comic) src)
	prefix (or (:prefix comic) (:url comic))]
    (str prefix image)))


(defmulti fetch-comic :type)
(defmethod fetch-comic :default [comic]
  (let [src (fetch-url (:url comic))
        image (re-find (:regex comic) src)
	prefix (or (:prefix comic) (:url comic))]
    (str prefix image)))

(doseq [comic *comics*]
  (println (str (:name comic) ": " (fetch-comic comic))))



(import '(org.htmlparser Parser)
        '(org.htmlparser.visitors NodeVisitor)
        '(org.htmlparser.tags ImageTag))

(defmethod fetch-comic :tooltip-comic [comic]
  (let [img-tags (ref [])
        parser (Parser. (:url comic))
        visitor (proxy [NodeVisitor] []
                  (visitTag [tag]
                            (when (and (instance? ImageTag tag)
                                       (re-find (:regex comic)
                                                (.getImageURL tag)))
                              (dosync (alter img-tags conj tag)))))]
    (.visitAllNodesWith parser visitor)
    [(.getImageURL (first @img-tags))
     (.getAttribute (first @img-tags) "title")]))




;(+ 3 4)
;(dialog "yo")

;(defmulti report empty?)
;(defmethod report true [x] "empty")
;(defmethod report :default [x] "elements")
;(report "")
;(report [1 2 3])


(defn dialog [message]
  (. javax.swing.JOptionPane (showMessageDialog nil message)))

(let [parser (Parser. "http://www.xkcd.com")
      visitor (proxy [NodeVisitor] []
		(visitTag [tag]
			  (when (instance? ImageTag tag)
			    (println (.getAttribute tag "title")(.getImageURL tag)))))]
  (.visitAllNodesWith parser visitor))

(def *thingy* (ref []))
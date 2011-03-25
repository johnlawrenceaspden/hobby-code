;; I like my html readable:
;; Nakkaya shows us the way in
;; http://nakkaya.com/2010/03/27/pretty-printing-xml-with-clojure/

(defn ppxml [xml]
  (let [in  (javax.xml.transform.stream.StreamSource. (java.io.StringReader. xml))
        out (javax.xml.transform.stream.StreamResult. (java.io.StringWriter.))
        transformer (.newTransformer 
                     (javax.xml.transform.TransformerFactory/newInstance))]
    (doto transformer
      (.setOutputProperty javax.xml.transform.OutputKeys/INDENT "yes")
      (.setOutputProperty "{http://xml.apache.org/xslt}indent-amount" "2")
      (.setOutputProperty javax.xml.transform.OutputKeys/METHOD "xml")
      (.transform in out))
    (-> out .getWriter .toString)))


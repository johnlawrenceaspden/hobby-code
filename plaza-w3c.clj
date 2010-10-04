(use 'plaza.rdf.core)
(import java.io.FileInputStream)

(reset-model) ;;It's imperative. Do this to flush all the old data

(def e (document-to-model
        (new FileInputStream
             "/home/john/hobby-code/w3schoolsrdfexamples/rdfdocumentexample.xml") :xml))

(def e-str (with-out-str (model-to-format e :n3)))

(print e-str)
(map #((.getURI (first %)) (second %) (nth % 2)) (model-to-triples e))
(.getURI (first (second (model-to-triples e))))

(->> e
     model-to-triples
     second
     first
     .getURI)


(.getURI (first (second (model-to-triples e))))

(reset-model)

(def e (document-to-model
        (new FileInputStream
             "/home/john/hobby-code/w3schoolsrdfexamples/rdfdocumentexample2.xml") :xml))

(meth-names  @e)

(. @e getGraph)

(map #(vector (.getLocalName (first %))(.getLocalName (second %))(.toString (nth % 2))) (model-to-triples e))

(reset-model)
(def e (document-to-model
        (new FileInputStream
             "/home/john/hobby-code/w3schoolsrdfexamples/rdfdocumentexample3.xml") :xml))


(map #(vector (.getLocalName (first %))(.getLocalName (second %))(.toString (nth % 2))) (model-to-triples e))



(use 'plaza.rdf.core)
(use 'plaza.rdf.predicates)

(reset-model)

(def rdfa (document-to-model
                 "http://www.learningclojure.com/"
                 :html))

(print (with-out-str (model-to-format rdfa :n1)))
(print (with-out-str (model-to-format rdfa :n2)))
(print (with-out-str (model-to-format rdfa :n3)))
(print (with-out-str (model-to-format rdfa :n4)))

(pp/pprint
 (make-triples [["http://example.org/John"
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                "http://xmlns.com/foaf/0.1/Person"]]))

[[#<ResourceImpl http://example.org/John>
  #<PropertyImpl http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
  #<ResourceImpl http://xmlns.com/foaf/0.1/Person>]]


(pp/pprint
 (register-rdf-ns :ex "http://example.org/"))
{"http://example.org/" :ex,
 "http://www.w3.org/2000/01/rdf-schema#" :rdfs,
 "http://www.w3.org/1999/02/22-rdf-syntax-ns#" :rdf}

(pp/pprint
 (register-rdf-ns :foaf "http://xmlns.com/foaf/0.1/"))

{"http://xmlns.com/foaf/0.1/" :foaf,
 "http://example.org/" :ex,
 "http://www.w3.org/2000/01/rdf-schema#" :rdfs,
 "http://www.w3.org/1999/02/22-rdf-syntax-ns#" :rdf}

(pp/pprint
 (make-triples [[[:ex :Alex] [:rdf :type] [:foaf :Person]]]))

(alter-root-rdf-ns "http://www.example.org/")

(def t (make-triples [[[:ex :Alex] [:rdf :type] [:foaf :Person]]]))
(s (first t)) ; subject
(p (first t)) ; predicate
(o (first t)) ; object

(resource-uri (o (first t)))
(literal-datatype-uri (d 5))
(literal-value (d 5))
(literal-language (l "abc" "en"))

(def m (defmodel (model-add-triples t)))

(def e (document-to-model "http://www.snee.com/rdf/elvisimp.rdf" :xml))

(def et (model-to-triples e))

(pp/pprint (take 10 (map #(p %) et)))

(take 10 (map #(resource-uri (p %)) et))

(sort (distinct (map #(resource-uri (p %)) et)))



(filter (triple-check (object-and? (is-literal?)
                                   (regex? #"impersonator"))) et)

(filter (triple-check (object-and? (is-literal?)
                                   (regex? #"[Cc]hinese"))) et)

(use 'plaza.rdf.sparql)

(def elvez (make-pattern [[:?s :?p (d "El Vez")]]))

(pattern-apply et elvez)

(def q (defquery
         (query-set-type :select)
         (query-set-vars [:?s :?p])
         (query-set-pattern elvez)))

(query-to-string q)

(model-query e q)

(sparql-to-query "SELECT ?s ?p WHERE {?s ?p \"El Vez\"}")


;;So, what are jena, sparql, rdf?






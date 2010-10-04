;; What is linked data?

;; RDF is a method of representing graphs of data

;; In one of its more memorably tedious passages, the Bible makes an early
;; attempt at linked data:

;; This is the book of the generations of Adam.  In the day that God created
;; man, in the likeness of God made he him.  Male and Female created he them;
;; and blessed them, and called their name Adam, in the day when they were
;; created.

;; And Adam lived an hundred and thirty years, and begat a son in his own
;; likeness, after his image; and called his name Seth: And the days of Adam
;; after he had begotten Seth were eight hundred years....

;; and so on. Genesis 5 suffers from a certain lack of formal precision, so let
;; us attempt to tidy the syntax up a little:

;; God created Adam
;; God created Eve
;; Eve mother of Seth
;; Adam father of Seth
;; Seth begat Enos
;; Enos begat Cainan

;; And we are back in the world of PROLOG. However PROLOG gives us no way of
;; understanding what its phrases actually mean.

;; RDF is a data format for expressing PROLOG-like statements
;; (subject, predicate, object) while specifying the meanings of the phrases
;; by giving places where the definitions can be looked up.

;; We might define God by his web address: http://www.heaven.com/SupremeBeing
;; And the fallen couple and their sons and grandsons their e-mails:
;; adam@earth.com, firstlady@earth.com, seth@gmail.com, etc.
;;
;; But we also need to specify what the relation predicates are:
;; What does begat mean, exactly? Perhaps the excellent hypothetical
;; http://www.familyrelationships.com/begat can help?

;; RDF data models can be expressed in many ways. One popular one is XML, and
;; the XML form of RDF seems to be commonly referred to as RDF, but there are
;; other more readable formats.

;; There is a Java library, Jena, for dealing with rdf data in its many forms
;; And it has been wrapped for clojure by the plaza library.

;; Plaza is on clojars. To use it you should add this snippet to your pom.xml

;;<dependency>
;;  <groupId>plaza</groupId>
;;  <artifactId>plaza</artifactId>
;;  <version>0.0.2-SNAPSHOT</version>
;;</dependency>

;; and it depends on some java code which can be found in this repository:

;;<repository>
;;  <id>jboss</id>
;;  <url>http://repository.jboss.org/nexus/content/groups/public/</url>
;;</repository>

;; If you add both these things to your usual pom.xml then you should be able to
;; evaluate:

(use 'plaza.rdf.core)

;; There is a site www.uk-postcodes.com which provides rdf data.

;; The postcode of my ancestral seat in Storrs is S66GY, let's see what we can learn.

;; We can get data in xml format with:
;; wget http://www.uk-postcodes.com/postcode/S66GY.rdf
;; Or we can use clojure and plaza to parse and examine it.

(def s66gy (document-to-model
            "http://www.uk-postcodes.com/postcode/S66GY.rdf"
            :xml))

;; XML being pretty much unreadable, let's see what it looks like in the n3
;; format:

(model-to-format s66gy :n3)

;; @prefix rdfs:    <http://www.w3.org/2000/01/rdf-schema#> .
;; @prefix geo:     <http://www.w3.org/2003/01/geo/wgs84_pos#> .
;; @prefix osadmingeo:  <http://data.ordnancesurvey.co.uk/ontology/admingeo/> .
;; @prefix spatialrelations:  <http://data.ordnancesurvey.co.uk/ontology/spatialrelations/> .
;; @prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
;; @prefix elecgeo:  <http://statistics.data.gov.uk/def/electoral-geography/> .
;; @prefix admingeo:  <http://statistics.data.gov.uk/def/administrative-geography/> .

;; <http://statistics.data.gov.uk/id/local-authority/00CG>
;;       rdfs:label "Sheffield City Council" .

;; <http://www.uk-postcodes.com/postcode/S66GY>
;;       spatialrelations:easting
;;               "428960"^^<http://www.w3.org/2001/XMLSchema#float> ;
;;       spatialrelations:northing
;;               "389341"^^<http://www.w3.org/2001/XMLSchema#float> ;
;;       spatialrelations:t_spatiallyInside
;;               <http://statistics.data.gov.uk/id/local-authority/00CG> , <http://statistics.data.gov.uk/id/electoral-ward/00CGHG> ;
;;       admingeo:localAuthority
;;               <http://statistics.data.gov.uk/id/local-authority/00CG> ;
;;       elecgeo:parliamentaryConstituency
;;               <http://statistics.data.gov.uk/id/parliamentary-constituency/398> ;
;;       elecgeo:ward <http://statistics.data.gov.uk/id/electoral-ward/00CGHG> ;
;;       geo:lat 53.400112 ;
;;       geo:long -1.5659 .

;; <http://statistics.data.gov.uk/id/electoral-ward/00CGHG>
;;       rdfs:label "Stannington" .


;; This is actually mildly interesting if you're me.

;; We have the map coordinates 428960 easting 389341 northing, which are
;; floating point numbers used according to the ordnance survey's spatial
;; relations ontology scheme.

;; And we have more widely used latitude and longitude 53.400112, -1.5659

;; We also know that Storrs is
;; is administered by the local authority 00CG, is in the electoral ward 00CGHG, and is in
;; the parliamentary constituency 398

;; and that it is spatially inside two of these entities, its local authority
;; and its electoral ward, although it is not asserted to be inside its
;; parliamentary constituency.

;; Similarly we can tell that 00CGHG is labelled Stannington, and 00CG is Sheffield City Council.


;; You'd imagine that somewhere in uk-postcodes.com is a data structure something like:

postcode {
easting
northing
lat
long
ward
authority
constituency
inside-list}

;; S66GY, 428960, 389341, 53.4, -1.5, 00CGHG, 00CG, 398, (00CGHG, 00CG)

;; But we'd have trouble understanding that it had been the reply, a parser
;; would need to be written, the schema would have needed a human to understand
;; it, etc.

;; Our rdf-xml snippet has brought its definitions with it.

;; Let's look at our data in a more program-friendly form, as a PROLOG-like set of triples:

;; We'll retrieve the data for my old college, King's: (funny how these old postcodes stick in the brain. I've no idea what the postcode of my current address is).

(def postcodetriples
     (model-to-triples (document-to-model
                        "http://www.uk-postcodes.com/postcode/CB21ST.rdf"
                        :xml)))

(first postcodetriples)
;;[#<ResourceImpl http://www.uk-postcodes.com/postcode/CB21ST>
;; #<PropertyImpl http://statistics.data.gov.uk/def/administrative-geography/localAuthority>
;; #<ResourceImpl http://statistics.data.gov.uk/id/local-authority/12UB>]

;; The first triple is telling us that CB2 1ST is in the local-authority 12UB.

;; What are all the relations described by our rdf?
(distinct (map #(resource-uri (p %)) postcodetriples))
("http://statistics.data.gov.uk/def/administrative-geography/localAuthority"
 "http://data.ordnancesurvey.co.uk/ontology/admingeo/CountyElectoralDivision"
 "http://data.ordnancesurvey.co.uk/ontology/spatialrelations/t_spatiallyInside"
 "http://statistics.data.gov.uk/def/electoral-geography/parliamentaryConstituency"
 "http://www.w3.org/2003/01/geo/wgs84_pos#long"
 "http://data.ordnancesurvey.co.uk/ontology/spatialrelations/northing"
 "http://data.ordnancesurvey.co.uk/ontology/spatialrelations/easting"
 "http://statistics.data.gov.uk/def/electoral-geography/ward"
 "http://www.w3.org/2003/01/geo/wgs84_pos#lat"
 "http://www.w3.org/2000/01/rdf-schema#label")



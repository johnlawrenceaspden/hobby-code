;; Indexing into a vector: a sequence of better answers
;; The other day, I had a csv file to read (csv files are comma-separated
;; values, often produced by exporting spreadsheets, or a reasonably standard
;; human-readable format for grids of data)


;; There's a lovely library which takes all the hard work out of reading csv files

;; The library's on clojars, so you use it by adding this to your maven classpath

;; <dependency>
;;   <groupId>clojure-csv</groupId>
;;   <artifactId>clojure-csv</artifactId>
;;   <version>1.2.0</version>
;; </dependency>

;; and then you can use clojure-csv/parse-csv to read your file in as a
;; sequence.  It seems to magically handle all the intricies of all the
;; different possible csv file arrangements.

;; But then I hit a problem: I wanted to read the email-addresses from every record.

;; I've rather simplified here, but let's imagine that the data looked like this:

(def csv-data
     [["Name" "E-mail 1" "Address" "E-mail 2" "E-mail 3"]
      ["John" "john@mailinator.com" "3 Round Church St" "xyz@learnclojure.com" ""]
      ["Fred" "fred@mailinator.com" "7 Park Street" "fred@gmail.com" "fred@googlemail.com" ]])

(def header (first csv-data))
(def data   (rest csv-data))

;; As it happened, most of the email-addresses were stored in the second column,
;; but some records had two or even three addresses, and it looked as though the
;; generating program might be labelling its columns according to the number of
;; e-mail addresses it needed to output.

;; It seemed very likely that another data set might have an "E-mail 4" column,
;; and it seemed unwise to rely on the needed columns always being 1,3,4 and
;; possibly 5. What if the program introduced another field entirely?

;; So obviously I needed a function to look up things in the header row somehow.
;; And there didn't seem to be one, although there were the interesting functions
;; keep-indexed and map-indexed, which I hadn't noticed before.

;; And I couldn't find one. So I figured that I could write one, or I could ask
;; on Stack Overflow.

;; And so I did both, expecting to find that I'd re-invent something in the
;; standard library, or at least in contrib, that I hadn't been able to find.

;; So the function(s) I came up with were:

(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn find-thing [value coll]
  (first-index-of #(= % value) coll))

;; And here are some examples:

(indices-of #(= "Name" %) header) ; (0)
(indices-of (partial re-matches #".*E-mail.*") header) ; (1 3 4)
(first-index-of #(= "Name" %) header) ; 0
(find-thing "Name" header) ; 0


;; But I was a bit nervous about these solutions, because I thought I must just
;; have re-invented some sort of wheel, and also because they're happy to return
;; answers for sets and maps, where the question doesn't really make much sense

;;fine
(find-thing "two" ["one" "two" "three" "two"]) ; 1
(find-thing "two" '("one" "two" "three")) ; 1

;;but these answers are a bit silly
(find-thing "two" #{"one" "two" "three"}) ; 1
(find-thing "two" {"one" "two" "two" "three"}) ; nil

;; But I went back to Stack Overflow, in order to answer my own question, and
;; found a couple of much better answers.

;; Brian Carper pointed out:

(.indexOf header "Name") ; 0

;; Which I hadn't noticed because my clever documentation-searching functions don't include
;; Java methods, and which is clearly the answer for searching vectors.

;; And XXXXXXX pointed out this lovely thing, originally due to Stuart Sierra:
(require 'clojure.contrib.seq)
(first (clojure.contrib.seq/positions #{"Name"} header))

;; positions is basically my indices-of, but using a set as the predicate is really clever.

;; anyway, now I could say:
(map #( % (.indexOf header "Name")) data)
(map #( % (.indexOf header "E-mail 1")) data)

;; Or even, for fans of crypticity, something like:
(map #(vector (% (.indexOf header "Name"))
              (for [i (clojure.contrib.seq/positions
                       (partial re-matches #"E-mail \d+") header)]
                (% i))) data)

;; But a little later, there was another answer from cgrand, who warned me
;; against using indices on general principles. And I agree with that, so I
;; asked what I should do in the particular case of csv files. And there was an
;; answer from Alex Stoddard, which I believe is the answer to the question that
;; I should have asked.

;; We can make a map from strings to indices
(def hmap (zipmap header (iterate inc 0)))

;; And use it like this:
(map #(% (hmap "Name")) data)

;; or this:
(def e-mails (filter (partial re-matches #"E-mail \d+") header))

(map #(vector (% (hmap "Name")) (for [e e-mails] (% (hmap e)))) data)

;; or this:
(map #(vector
       (% (hmap "Name"))
       (for [e (filter (partial re-matches #"E-mail \d+") header)]
         (% (hmap e)))) data)

;; or even this (although now you do have to rely on the name coming before the e-mails):
(map #(for [e (filter (partial re-matches #"E-mail \d+|Name") header)]
        (% (hmap e)))
     data)

;;If we want to abstract out a pattern, then:

;; either:
(defn columns [f header data]
  (for [e (clojure.contrib.seq/positions f header)]
    (map #(% e) data )))

;; or:
(defn columns [f header data]
  (let [hmap (zipmap header (iterate inc 0))]
    (map #(for [e (filter f header)] (% (hmap e))) data)))

;; allow:
(zipmap
 (apply concat (columns #{"Name"} header data))
 (columns (partial re-matches #"E-mail \d+") header data))








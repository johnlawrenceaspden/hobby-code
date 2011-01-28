;; Indexing into a vector: a sequence of better answers

(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn find-thing [value coll]
  (first-index-of #(= % value) coll))

(find-thing "two" ["one" "two" "three" "two"]) ; 1
(find-thing "two" '("one" "two" "three")) ; 1

;; these answers are a bit silly
(find-thing "two" #{"one" "two" "three"}) ; 1
(find-thing "two" {"one" "two" "two" "three"}) ; nil

(require 'clojure.contrib.seq)
(first (clojure.contrib.seq/positions #{"Name"} gmail-job-spam-list-header))


(map #(nth % (find-thing "Name" gmail-job-spam-list-header)) gmail-job-spam-list-csv)
(map #(nth % (find-thing "E-mail 1 - Type" gmail-job-spam-list-header)) gmail-job-spam-list-csv)


(let [raw (parse-csv gmail-job-spam-list-csv)]
  (def gmail-job-spam-list-header (first raw))
  (def gmail-job-spam-list-csv    (rest raw)))



(def header (zipmap gmail-job-spam-list-header (iterate inc 0)))
(map #(nth %  (header "Name") ) gmail-job-spam-list-csv)


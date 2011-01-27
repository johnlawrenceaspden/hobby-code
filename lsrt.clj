(defn as-file [x]
  (cond (= (class x) java.io.File) x
        (string? (name x)) (java.io.File. (name x))
        :else  (throw "eek")))

(as-file ".")
(as-file (java.io.File. "."))

(defn file? [x]
  (= (class (as-file x)) java.io.File))

(defn get-files [dir]
     (seq (.listFiles (as-file dir))))

(get-files (java.io.File "."))

(defn lsrt* [dir n]
  (take n (map #(.getName %) (sort-by #(.lastModified %) (get-files dir)))))

(lsrt* '. 10)
(lsrt* "." 10)
(lsrt* (java.io.File. ".") 10)

(defmacro lsrt
  ([a b]
     (cond
      (and (symbol? a) (number? b)) `(lsrt* (as-file '~a) ~b)
      (and (number? a) (symbol? b)) `(lsrt* (as-file '~b) ~a)
      :else  `(lsrt* (as-file ~a) ~b))))

(lsrt . 10)
(lsrt "." 10)
(lsrt (java.io.File. ".") 10)

(file? '.)
(number? '10)
(as-file '.)

(lsrt (java.io.File. ".") 10)
(lsrt* "." 10)

(as-file (java.io.File "."))

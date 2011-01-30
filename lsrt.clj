;; I often want to find out what the most recent things I've changed are, and it
;; annoys me to have to leave my REPL to find out, so I added this to my startup
;; file:

(defn as-file [x]
  (try
    (cond (= (class x) java.io.File) x
          (string? (name x)) (java.io.File. (name x))
          :else  (throw (Exception. "?")))
    (catch Throwable t (throw (Exception. (str x " is not a file."))))))

(defn file? [x]
  (= (class (as-file x)) java.io.File))

(defn get-files [dir]
     (seq (.listFiles (as-file dir))))

(defn lsrt* [dir n]
  (take n (map #(.getName %) (reverse (sort-by #(.lastModified %) (get-files dir))))))

;; Up until this point it's all kind of sensible, and I can do
(def curdir (java.io.File. "."))

(lsrt* '. 10)
(lsrt* "." 10)
(lsrt*  curdir 10)

;; But I find that interactively, I am always forgetting quotes or typing things
;; in the wrong way round, and for this function I'd prefer 'do what you think I
;; mean':

(defmacro lsrt
  ([a]
     (cond (number? a) `(lsrt* "." ~a)
           (symbol? a) `(lsrt* '~a 10)
           :else `(lsrt* (as-file ~a) 10)))
  ([a b]
     (cond (and (number? a) (symbol? b)) `(lsrt* (as-file '~b) ~a)
           (number? a) `(lsrt* (as-file ~b) ~a)
           (symbol? a) `(lsrt* (as-file '~a) ~b)
           :else  `(lsrt* (as-file ~a) ~b))))

(lsrt . 10)
(lsrt 10 .)
(lsrt .)
(lsrt (java.io.File. "."))
(lsrt 10)
(lsrt "." 10)
(lsrt 10 ".")
(lsrt (java.io.File. ".") 10) ; 
(lsrt 10 (java.io.File. "."))

;unfortunately:

(lsrt curdir) ; ()
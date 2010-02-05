;; glob.clj -- globbing support for Clojure

;; A web search produced this:
;; http://svn2.assembla.com/svn/iode/src/compojure/lib/compojure/glob.clj
;; which appeared to be a very old part of compojure
;; It didn't work as found, so hackety hack...

(ns glob
  (:use clojure.test)
  (:import java.io.File))

(defn file
  "Returns an instance of java.io.File."
  ([name]          (new File name))
  ([parent name]   (new File parent name))
  ([p q & parents] (reduce file (file p q) parents)))

(defn file-parents
  "Lazily iterate through all of the parents of a file."
  [file]
  (take-while identity
    (iterate (memfn getParentFile) file)))

(defn split-path
  "Splits a path up into its parts."
  [path]
  (map
    (memfn getName)
    (reverse
      (file-parents (file path)))))

(defn includes?
  "Returns true if s contains something equal (with =) to x."
  [x s]
  (if (some (fn [y] (= y x)) s)
    true false))

(defn escape
  "Returns a string with each occurence of a character in 'chars' escaped with a \\ ."
  [chars #^String string]
  (apply str
    (mapcat
      #(if (includes? % chars) [\\ %] [%])
      string)))

(defn- glob->regex
  "Turns a shallow file glob into a regular expression."
  [s]
  (re-pattern
    (.. (escape "\\.+|()[]{}$^" s)
        (replaceAll "\\*" ".*")
        (replaceAll "\\?" "."))))

(map glob->regex (list "*" "?" "h?ll*" "*?*??" "*.clj" "(c+e[q]u*_il)"))
(glob->regex "*.clj")
(glob->regex "*.clj")

(defn- recursive-glob?
  [glob]
  (re-find #"\\*\\*" glob))

(defn- glob-parts
  [parts path]
  (if (seq parts)
    (if (. path isDirectory)
      (mapcat
       #(glob-parts (rest parts) %)
        (filter
         #(re-matches
            (glob->regex (first parts))
            (. % (getName)))
          (if (recursive-glob? (first parts))
            (file-seq path)
            (. path (listFiles))))))
    (list path)))

(glob-parts '( "*") (file "."))
(glob->regex "*e*_*e*.clj")
(. (file ".") isDirectory)


(defn glob
  "Find all files in a directory matching a glob."
  ([pattern]
    (glob-parts (split-path pattern) (file "."))))

(deftest globbing
  (is (> (count (glob "*")) 0))
  (is (apply > (map count 
                    (list 
                     (glob "*") (glob "*.clj") (glob "*e*.clj") 
                     (glob "*e*e*.clj") (glob "*e*_*e*.clj") (glob "*e??????e*.clj"))))))



  (comment
    ([path pattern]
       (map
        #(relative-path path %)
        (glob-parts (split-path pattern) (file path)))))

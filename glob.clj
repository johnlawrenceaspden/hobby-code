;; glob.clj -- globbing support for Clojure


;; A web search produced this:
;; http://svn2.assembla.com/svn/iode/src/compojure/lib/compojure/glob.clj
;; which appeared to be a very old part of compojure.  It didn't work
;; as found, so hackety hack...

(ns glob
  "globbing support"
  (:use clojure.test)
  (:import java.io.File))

(defn file
  "Returns an instance of java.io.File."
  ([name]          (new File name))
  ([parent name]   (new File parent name))
  ([p q & parents] (reduce file (file p q) parents)))

;;This is getting the parents of the file by creating File objects
(defn file-parents
  "Lazily iterate through all of the parents of a file."
  [file]
  (take-while identity
              (iterate (memfn getParentFile) file)))

;;Similarly using file objects
(defn split-path
  "Splits a path up into its parts. Creates a file object and then examining it."
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
  "Returns a string with each occurence of a character in 'chars' escaped with a backslash ."
  [chars #^String string]
  (apply str
         (mapcat
          #(if (includes? % chars) [\\ %] [%])
          string)))

                                        ;(escape "()" "hello(world)")

(defn- glob->regex
  "Turns a shallow file glob into a regular expression."
  [s]
  (re-pattern
   (.. (escape "\\.+|()[]{}$^" s)
       (replaceAll "\\*" ".*")
       (replaceAll "\\?" "."))))

(defn- recursive-glob?
  "does the glob have a ** in it?"
  [glob]
  (not (empty? (re-find #"\*\*" glob))))

(map recursive-glob? '("*.clj" "**/*.clj" "*/*"))

(defn- glob-parts
  [parts path]
  (if (seq parts)
    (if (. path isDirectory)
      (mapcat
       #(glob-parts (rest parts) %)
       (filter
        #(re-matches (glob->regex (first parts)) (. % (getName)))
        (if (recursive-glob? (first parts))
          (file-seq path)
          (. path (listFiles))))))
    (list path)))


;;glob-parts should take a glob split into pieces, e.g. "**" "*" "hello*" "*.clj"
;;and a path.

;;if the path is a file, then it should match if there is only one glob part, and it matches the regexp
;;if it is a directory, then  

(defn- glob-parts [parts path]
  (if (empty? parts) (list path)
      (


(defn glob
  "Find all files in a directory matching a glob."
  ([pattern]
     (glob-parts (split-path pattern) (file ".")))
  ([path pattern]
     (glob-parts (split-path pattern) (file path))))


(use 'clojure.contrib.trace)
(dotrace (glob-parts glob) (glob "maven/**/*.clj"))










(glob "." "")
(glob-parts '() (file "."))
(split-path "d")

(map count (list 
            (glob "/home/john/hobby-code" "**/*.clj")
            (glob "/home/john/hobby-code" "*/*.clj")
            (glob "/home/john/hobby-code" "*/*.clj")))


(map glob->regex (list "*" "?" "h?ll*" "*?*??" "*.clj" "(c+e[q]u*_il)"))

(deftest globbing
  (is (> (count (glob "*")) 0))
  (is (apply > (map count 
                    (list 
                     (glob "*") (glob "*.clj") (glob "*e*.clj") 
                     (glob "*e*e*.clj") (glob "*e*_*e*.clj") (glob "*e??????e*.clj"))))))



(comment "regular expression experiments"
         (re-pattern "foo")             ;string->pattern
         (re-matcher #"foo" "foofoofoo") ; makes matcher thingy
         (re-seq #"foo" "foofoofoo") ("foo" "foo" "foo") ;;returns sequence of matches
         (re-seq #"fo+?" "foofoofoo") ("fo" "fo" "fo")

         (def m (re-matcher (re-pattern "foo") "foofooofoooo"))
         (re-find m)
         (re-groups m)

         (re-matches #"(fo)o*" "foofoofooo")
         (re-seq #"(foo)\1o*" "foofoofooo")

         (re-find #"foo*" "foofoofooo"))

(comment "glob experiments"

         (glob-parts '( "*") (file "."))
         (glob->regex "*e*_*e*.clj")
         (. (file ".") isDirectory))




;;I'm interested in transforming a file from clojure code
;;to html in a way that allows me to write ordinary code 
;;that becomes blog posts about itself.

;;For long discursive comments, I tend to use lines
;;starting with double ; and leave blank lines with
;;in between paragraphs

;;comment lines occasionally have embedded lisp (like this)
;;and sometimes have parenthetical comments (like this)
;;I suspect it may be difficult to tell these apart.

;;so i'd be prepared to change my style 
(* 2 2) 
;;when I'm talking about a list expression, but I'd still like this sort
;;of thing to translate nicely into html

;;As well as that there's actual program code, which I want nicely
;;labelled with <div>, <pre>, <code class="clojure"> tags.

;;And I must remember to escape < to &gt;, or whatever it should be in html.

;;Of course, there's sometimes real code too
(def alf (map char (range 97 (+ 97 26))))

;;There's the definition of the alphabet, and here's a function
(mapcat (fn[[d e f]] 
       (map (fn[[a b c]]
              (str a b c \\ d e f)) ;;just occasionally you get a comment like this.
            (partition 3 1 alf)))
     (partition 3 2 alf))

;;So it looks like there are several types of lines

;;comments, blanks, lisp code, 

;;continuous runs of code should be quoted with <code><div><pre>

;;single lines of code with comments either side should get <code> tags and html-quoting

;;Comments should have their comment-markers stripped

;;blank lines with comments either side should be new paragraphs

;;all dangerous characters everywhere should be html-quoted

(def file (slurp "/home/john/clojure-programs/threefor.clj"))

(use 'clojure.contrib.str-utils)


(def lines (clojure.contrib.str-utils/re-split #"\n" file))

(defn blank? [line] (every? #(= % \space) line))
;; Here's how I might test blank 
(map blank? '("" "  " " " ";" "hello"))
;; <-- this should have appeared as a piece of code in the text.

(defn comment? [line] (and (> (count line) 0) (= (nth line 0) \;)))
(defn comment->text [line] 
  (if (comment? line) (recur (apply str (drop 1 line))) line))

;; here's a test for comment
(map comment->text  '(";hello" ";" "" "no" ";;;biggie"))
;;

(defn classify-line [line]
  (cond (blank? line) :blank
        (comment? line) :comment
        true :lisp))

(def classifications (map classify-line lines))

;;Schlemiel the painter
(defn triples [classifications]
     (concat (list [:begin (first classifications) (second classifications)])
             (partition 3 1 classifications)
             (list [(last (butlast classifications)) (last classifications) :end])))


(count lines)
(count (triples classifications))


(defn classify-triple [triple]
  (cond (= triple [:comment :lisp :comment]) :comment-lisp
        true (second triple)))

(def labelled-lines (partition 2 (interleave (map classify-triple (triples classifications)) lines)))

(require 'clojure.contrib.seq-utils)

(def groups (clojure.contrib.seq-utils/partition-by first labelled-lines))

(count lines)
(count labelled-lines)
(count groups)

(defn group->tagged-group [group]
     [(first (first group)) (map second group)])

(nth groups 0)

(def tagged-groups (map group->tagged-group groups))

(map first tagged-groups)



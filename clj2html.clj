;;Turns clojure code into  html

;;Takes all comments on their own line and converts them to text paragraphs
;;Takes all live code to <div><pre><code class="clojure">...</></></>
;;sections.

(use 'clojure.contrib.str-utils)
(use 'clojure.contrib.seq-utils)
(use 'clojure.contrib.duck-streams)

(def file (slurp (first *command-line-args*)))
;(def file (slurp "/home/john/hobby-code/clj2html.clj"))

(def lines (clojure.contrib.str-utils/re-split #"\n" file))

(defn blank? [line] (every? #(= % \space) line))
;(map blank? '("" "  " " " "; " "hel lo"))

(defn comment? [line] (and (> (count line) 0) (= (first line) \;)))

(defn comment->text "remove leading ;;"
  [line] 
  (if (comment? line) (recur (apply str (drop 1 line)))
      line))
;(map comment->text  '(";hello" ";" "" "no" ";;;biggie"))

(def analysed-lines (map (fn [line] [(comment? line) line]) lines))

(def groups (partition-by (fn [line] (comment? line)) lines))

(defn transform-group [group]
     (if (comment? (first group))
       (concat '("<p>")
               (map comment->text group) 
               '("</p>"))
       (if (every? #(blank? %) group)
         group
         (concat '("<div><pre><code class=\"clojure\">") 
                 group 
                 '("</div></pre></code>")))))

(doall (map println (mapcat transform-group groups)))
;publish clojure code

(use 'clojure.contrib.str-utils)
(use 'clojure.contrib.seq-utils)
(use 'clojure.contrib.duck-streams)

(def file (slurp (first *command-line-args*)))

(def lines (clojure.contrib.str-utils/re-split #"\n" file))

(defn blank? [line] (every? #(= % \space) line))
;(map blank? '("" "  " " " ";" "hello"))

(defn comment? [line] (and (> (count line) 0) (= (nth line 0) \;)))
(defn comment->text [line] 
  (if (comment? line) (recur (apply str (drop 1 line))) line))

;(map comment->text  '(";hello" ";" "" "no" ";;;biggie"))

(def analysed-lines (map (fn [line] [(comment? line) line]) lines))

(def groups (partition-by (fn [line] (comment? line)) lines))

(defn transform-group [group]
     (if (comment? (first group))
       (concat '("<p>") (map comment->text group) '("</p>"))
       (if (every? #(blank? %) group)
         group
         (concat '("<div><pre><code class=\"clojure\">") group '("</div></pre></code>")))))

(doall (map println (mapcat transform-group groups)))

;; (defn parse-lines [lines] (parse lines true '[]))

;; (defn parse [lines in-comment? acc]
;;      (if (= (count lines) 0)
;;        acc
;;        (let 
;;            [line (first lines)
;;             comment-line (comment? line)]
;;          (cond (and (not in-comment?) comment-line)
;;                  (recur (rest lines) true (conj acc (comment->text line)))
;;                (and in-comment? comment-line)
;;                  (recur (rest lines) true (conj acc (comment->text line)))
;;                (and (not in-comment?) (not comment-line))
;;                  (recur (rest lines) false (conj acc line))
;;                (and in-comment? (not comment-line))
;;                  (recur (rest lines) false (conj acc line)))))))
;;
;;(parse-lines lines)


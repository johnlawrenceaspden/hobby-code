(import '(java.io File PrintWriter))

;; Helpers ;;

; Copied from clojure.contrib.duck-streams because I'm evil
(defn spit
  "Opposite of slurp.  Opens f with writer, writes content, then
  closes f."
  [f content]
  (with-open [#^PrintWriter w (PrintWriter. f)]
      (.print w content)))

(defn third [coll] (second (next coll)))

;; Formatting ;;

(def whitespace? #{\,\newline\tab\space})
(def starting? #{\(\#\{\"\[\;\\})
(def ending? #{\)\"\}\]\;})
(defn special-char? [c] (or (whitespace? c) (starting? c) (ending? c)))

(defn html-entity [c] (get {\< "&lt;" \> "&gt;" \& "&amp;"} c c))
(defn html-whitespace [c] (get {\newline "<br>" \tab "&nbsp;&nbsp;&nbsp;&nbsp;" \space "&nbsp;"} c c))

(def html-safe (comp html-whitespace html-entity))

;; Parsing ;;

(defstruct html-tag :type :started?)
(defn assoc-first [coll & args] (conj (rest coll) (apply assoc (first coll) args)))

(defn parse-code [code]
    (loop [result (StringBuilder.)
           code (seq code)
           tags '()
           defs #{}]
        (let [tag (first tags)]
            (if (empty? code)
                (.toString result)
                (cond    
                    ; Comment
                    (= (:type tag) :comment)
                        (if (and (not (:started? tag)) (= (first code) \;))
                            (recur (.append result (str "<span class=\"clojure-comment\">;"))
                                   (rest code)
                                   (assoc-first tags :started? true)
                                   defs)
                            (if (= (first code) \newline)
                                (recur (.append result "</span>") code (rest tags) defs)
                                (recur (.append result (html-safe (first code))) (rest code) tags defs)))
                    
                    ; String
                    (= (:type tag) :string)
                        (cond
                            (= (first code) \")
                                (if (:started? tag)
                                    (recur (.append result "\"</span>") (rest code) (rest tags) defs)
                                    (recur (.append result "<span class=\"clojure-string\">\"")
                                           (rest code)
                                           (assoc-first tags :started? true)
                                           defs))
                            (= (first code) \\)
                                (recur (.append result (str (first code) (second code))) (rest (rest code)) tags defs)
                            true
                                (recur (.append result (html-safe (first code))) (rest code) tags defs))
                    
                    ; Escaped Character
                    (= (:type tag) :escaped)
                        (if (and (not (:started? tag)) (= (first code) \\))
                            (recur (.append result (str "<span class=\"clojure-escaped\">\\" (second code)))
                                   (rest (rest code))
                                   (assoc-first tags :started? true)
                                   defs)
                            (if (special-char? (first code))
                                (recur (.append result "</span>") code (rest tags)defs)
                                (recur (.append result (first code)) (rest code) tags defs)))
                    
                    ; Vector
                    (and (= (:type tag) :vector) (not (:started? tag)) (= (first code) \[))
                        (recur (.append result "[<span class=\"clojure-vector\">")
                               (rest code)
                               (assoc-first tags :started? true)
                                defs)
                    (and (= (:type tag) :vector) (= (first code) \]))
                        (recur (.append result "</span>]") (rest code) (rest tags) defs)
                        
                    ; Set
                    (and (= (:type tag) :set) (not (:started? tag)) (= (first code) \#))
                        (recur (.append result "<span class=\"clojure-set\">#")
                               (rest code)
                               (assoc-first tags :started? true)
                                defs)
                    (and (= (:type tag) :set) (= (first code) \}))
                        (recur (.append result "}</span>") (rest code) (rest tags) defs)
                        
                    ; Keyword
                    (= (:type tag) :keyword)
                        (if (and (not (:started? tag)) (= (first code) \:))
                            (recur (.append result (str "<span class=\"clojure-keyword\">:" (second code)))
                                   (rest (rest code))
                                   (assoc-first tags :started? true)
                                   defs)
                            (if (special-char? (first code))
                                (recur (.append result "</span>") code (rest tags) defs)
                                (recur (.append result (first code)) (rest code) tags defs)))
                    
                    ; Meta Tag (read through maps)
                    (and (= (:type tag) :meta-tag) (not (:started? tag)) (= (first code) \#))
                        (recur (.append result (str "<span class=\"clojure-meta-tag\">#"))
                               (rest code)
                               (assoc-first tags :started? true)
                               defs)
                    (and (= (:type tag) :meta-tag) (:started? tag) (:reading-map? tag) (= (first code) \}))
                        (recur (.append result (first code)) (rest code) (assoc-first tags :reading-map? false) defs)
                    (and (= (:type tag) :meta-tag) (:started? tag) (= (first code) \{))
                        (recur (.append result (first code)) (rest code) (assoc-first tags :reading-map? true) defs)
                    (and (= (:type tag) :meta-tag) (:started? tag) (not (:reading-map? tag)) (special-char? (first code)))
                        (recur (.append result "</span>") code (rest tags) defs)
                    
                    ; Function List
                    (and (= (:type tag) :func-list) (not (:started? tag)) (= (first code) \())
                        (recur (.append result "<span class=\"clojure-list\">(")
                               (rest code)
                               (if (starting? (second code))
                                   (assoc-first tags :started? true)
                                   (conj (assoc-first tags :started? true) (struct html-tag :function false)))
                                defs)
                    (and (= (:type tag) :func-list) (= (first code) \)))
                        (recur (.append result ")</span>") (rest code) (rest tags) defs)
                        
                    ; List
                    (and (= (:type tag) :list) (not (:started? tag)) (= (first code) \())
                        (recur (.append result "<span class=\"clojure-list\">(")
                               (rest code)
                               (assoc-first tags :started? true)
                               defs)
                    (and (= (:type tag) :list) (= (first code) \)))
                        (recur (.append result ")</span>") (rest code) (rest tags) defs)
                    
                    ; Function Name
                    (= (:type tag) :function)
                        (if (:started? tag)
                            (if (special-char? (first code))
                                (cond
                                    (.startsWith (.toString (:value tag)) "def")
                                        (recur (.append result (str "<span class=\"clojure-function\">" (:value tag) "</span>"))
                                               code
                                               (conj (rest tags) (struct html-tag :def false))
                                               defs)
                                    (contains? defs (:value tag))
                                        (recur (.append result (str "<span class=\"clojure-function-known\">" (:value tag) "</span>")) code (rest tags) defs)
                                    true
                                        (recur (.append result (str "<span class=\"clojure-function\">" (:value tag) "</span>")) code (rest tags) defs))
                                (recur result
                                       (rest code)
                                       (assoc-first tags :value (str (:value tag) (first code)))
                                       defs))
                            (if (whitespace? (first code))
                                (recur (.append result (html-safe (first code))) (rest code) tags defs)
                                (recur result
                                       (rest code)
                                       (assoc-first tags :started? true :value (first code))
                                       defs)))
                    
                    ; Def
                    (= (:type tag) :def)
                        (if (:started? tag)
                            (if (special-char? (first code))
                                (recur (.append result (str "<span class=\"clojure-def\">" (:value tag) "</span>"))
                                    code
                                    (rest tags)
                                    (conj defs (:value tag)))
                                (recur result
                                       (rest code)
                                       (assoc-first tags :value (str (:value tag) (first code)))
                                       defs))
                            (cond
                                (whitespace? (first code))
                                    (recur (.append result (html-safe (first code))) (rest code) tags defs)
                                (= (first code) \#)
                                    (recur result code (conj tags (struct html-tag :meta-tag false)) defs)
                                true
                                (recur result
                                       (rest code)
                                       (assoc-first tags :started? true :value (first code))
                                       defs)))
                    
                    ; Clojure Reserved Characters
                    (= (first code) \\) (recur result code (conj tags (struct html-tag :escaped false)) defs)
                    (= (first code) \") (recur result code (conj tags (struct html-tag :string false)) defs)
                    (= (first code) \;) (recur result code (conj tags (struct html-tag :comment false)) defs)
                    (= (first code) \() (recur result code (conj tags (struct html-tag :func-list false)) defs)
                    (= (first code) \:) (recur result code (conj tags (struct html-tag :keyword false)) defs)
                    (= (first code) \[) (recur result code (conj tags (struct html-tag :vector false)) defs)
                    (= (first code) \') (recur (.append result \') (rest code) (conj tags (struct html-tag :list false)) defs)
                    (and (= (first code) \#) (= (second code) \^) (not (special-char? (third code))))
                        (recur result code (conj tags (struct html-tag :meta-tag false)) defs)
                    (and (= (first code) \#) (= (second code) \{))
                        (recur result code (conj tags (struct html-tag :set false)) defs)
                    
                    true
                        (recur (.append result (html-safe (first code))) (rest code) tags defs))))))

(defn pre-format [text]
    (.replace text "\r" ""))

;; Main ;;
(let
    [filename (first *command-line-args*)]
    (spit (str filename ".html")
        (str "<link type=\"text/css\" rel=\"stylesheet\" href=\"style.css\">"
        "<div class=\"clojure\">" (parse-code (pre-format (slurp filename))) "</div>")))

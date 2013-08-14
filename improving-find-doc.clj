;; Improving find-doc

;; Clojure could be self documenting, but in practice the
;; self-documentation functions are so little use that I find myself
;; using google and grep instead.

;; For instance, suppose we want to find out whether there's a heap built in to clojure
;; For some applications this is a better data structure than a sorted set.

;; These two functions work well, if you know the name of the thing you want.
(doc map)
(source map)
;; But if you don't:
(doc heap) ;-> nil

;; What I almost always try next is:
(find-doc heap) 
(find-doc 'heap)
;; The error for that tells me find-doc takes a string, so I should try
(find-doc "heap") ;-> nil
;; Sometimes heaps are called priority maps, or priority queues
(find-doc "priority") ;-> nil
;; But that doesn't work either!

;; At this point you google for "clojure priority heap", and that
;; points you at priority-map in contrib 1.3, which no longer exists,
;; but that's enough of a clue to google "clojure priority-map"
;; and eventually you find 
(require 'clojure.data.priority-map)
;; except that doesn't work because it's a separate repository all its own
;; so you kill your repl and you add
[org.clojure/data.priority-map "0.0.2"]
;; to project.clj and then you run leiningen again and it churns a bit
;; but if you are lucky then you can now do
(require 'clojure.data.priority-map)
;; and now your morning is gone but finally and rather humorously:
(find-doc "heap")
;; Now works, and tells you everything you need to know....
;;-------------------------
;;clojure.data.priority-map
;;  A priority map is very similar to a sorted map, but whereas a sorted map produces a
;;sequence of the entries sorted by key, a priority map produces the entries sorted by value.
;;In addition to supporting all the functions a sorted map supports, ........


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So what do we need to sort?

;; Well the first, rather trivial, problem is to make find-doc work whether
;; you type (find-doc heap) or (find-doc 'heap) or (find-doc "heap")

(defn stringify [x] 
  (cond (= (type x) (type #"")) x 
        (and (list? x) (= (first x) 'quote)) (str (second x)) 
        :else (str x)))

(defmacro fd [symbol-or-string] `(find-doc (stringify '~symbol-or-string)))

;; And that already makes me a lot happier because I can type all these things
;; and they will find the heap, given that it has already been found and required.
(fd heap)
(fd "heap")
(fd 'heap)
(fd #"he.p")

;; The output from find-doc be overwhelming.
;; This is like find-doc, but just gives the associated names.

(defn find-doc-names
  "Prints the name of any var whose documentation or name contains a match for re-string-or-pattern"
  [re-string-or-pattern]
  (let [re  (re-pattern re-string-or-pattern)]
    (doseq [ns (all-ns)
            v (sort-by (comp :name meta) (vals (ns-interns ns)))
            :when (or (re-find (re-matcher re (str (:name (meta v)))))
                      (and (:doc (meta v))
                           (re-find (re-matcher re (:doc (meta v))))))]
               (print v "\n"))))

(defn find-doc-names
  "Prints the name of any var whose documentation or name contains a match for re-string-or-pattern"
  [re-string-or-pattern]
    (let [re (re-pattern re-string-or-pattern)
          ms (concat (mapcat #(sort-by :name (map meta (vals (ns-interns %))))
                             (all-ns))
                     (map namespace-doc (all-ns))
                     (map special-doc (keys special-doc-map)))]
      (doseq [m ms
              :when (and (:doc m)
                         (or (re-find (re-matcher re (:doc m)))
                             (re-find (re-matcher re (str (:name m))))))]
               (print m))))




;; And here's and abbreviation for it
(defmacro fdn [symbol-or-string] `(find-doc-names (stringify '~symbol-or-string)))

(fdn "heap")
(fd "heap")


;;find symbol or string in docs 
(defmacro fd [symbol-or-string] `(find-doc (stringify '~symbol-or-string)))














(require 'cemerick.pomegranate)
(cemerick.pomegranate/add-dependencies :coordinates '[[org.clojure/data.priority-map "0.0.2"]])




;; So the first thing to fix is 
;; given an attempt to say something, turn it into a string




;; Sometimes I like to ask which public functions a namespace provides.
(defn ns-publics-list [ns] (#(list (ns-name %) (map first (ns-publics %))) ns))
;; And occasionally which functions it pulls in (with refer or use)
(defn ns-refers-list  [ns] (#(list (ns-name %) (map first (ns-refers %))) ns))

;; the specific things in the user namespace
(ns-publics-list *ns*)
;; everything you can refer to unqualified, so most of clojure.core
(ns-refers-list *ns*)


;; Nice pretty-printed versions of these functions, accepting strings, symbols or quoted symbol
(defmacro list-publics     
  ([]   `(pprint (ns-publics-list *ns*)))
  ([symbol-or-string] `(pprint (ns-publics-list (find-ns (symbol (stringify '~symbol-or-string)))))))

(defmacro list-refers
  ([]   `(pprint (ns-refers-list *ns*)))
  ([symbol-or-string] `(pprint (ns-refers-list (find-ns (symbol (stringify '~symbol-or-string)))))))

(list-publics)
(list-refers)


;; List all the namespaces
(defn list-all-ns [] (pprint (sort (map ns-name (all-ns)))))

(list-all-ns)

;; List all public functions in all namespaces!
(defn list-publics-all-ns [] (pprint (map #(list (ns-name %) (map first (ns-publics %))) (all-ns))))

(list-publics-all-ns)


(fd "queue")
(fdn "queue")
(fd peek)



(defmacro def-let
  "like let, but binds the expressions globally."
  [bindings & more]
  (let [let-expr (macroexpand `(let ~bindings))
        names-values (partition 2 (second let-expr))
        names (map first names-values)
        defs   (map #(cons 'def %) names-values)]
    (concat (list 'do) (list (list 'println (list 'quote (list* 'ns-unmap '*ns* names)))) defs more)))


(defmacro def-let
  "like let, but binds the expressions globally."
  [bindings & more]
  (let [let-expr (macroexpand `(let ~bindings))
        names-values (partition 2 (second let-expr))
        names (map first names-values)
        defs   (map #(cons 'def %) names-values)]
    (let [unmapexprlist  (map #(list 'ns-unmap '*ns* (list 'quote %)) names)
          printexpression (list (list 'println (list 'quote (list* 'do unmapexprlist))))]
      (concat (list 'do)  printexpression defs more))))


(defmacro def-let
  "like let, but binds the expressions globally."
  [bindings & more]
  (let [let-expr (macroexpand `(let ~bindings))
        names-values (partition 2 (second let-expr))
        names (map first names-values)
        defs   (map #(cons 'def %) names-values)]
    (let [unmapexprlist  (map #(list 'ns-unmap '*ns* (list 'quote %)) names)
          unmapexpression (list* 'do unmapexprlist)
          unmapfn (list 'fn [] unmapexpression)
          printexpression (list (list 'println (list 'quote unmapexpression)))]
      (concat (list 'do)  unmapfn printexpression defs more))))


(defmacro def-let
  "like let, but binds the expressions globally."
  [bindings & more]
  (let [let-expr (macroexpand `(let ~bindings))
        names-values (partition 2 (second let-expr))
        names (map first names-values)
        defs   (map #(cons 'def %) names-values)]
    (let [unmapexprlist  (map #(list 'ns-unmap '*ns* (list 'quote %)) names)
          unmapexpression (list* 'do unmapexprlist)
          unmapfn (list 'defn 'remove-crapspray [] unmapexpression)
          printexpression (list (list 'println "undo fn:" (list 'quote unmapfn)))]
      (concat (list 'do)  (list unmapfn) printexpression defs more))))






(def-let [a 3 b 2] (* a b)) 

(macroexpand '(def-let [a 3 b 2] (* a b)))




















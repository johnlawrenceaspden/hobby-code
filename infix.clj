(def AND #(and %1 %2))

(def rank (zipmap [- + * / AND =] (iterate inc 1)))

(defn infix* [[a b & [c d e & more]]]
  (cond
   (vector? a) (infix* (list* (infix* a) b c d e more))
   (vector? c) (infix* (list* a b (infix* c) d e more))
   (ifn? b) (if (and d (< (rank b 0) (rank d 0)))
              (infix* (list a b (infix* (list* c d e more))))
              (infix* (list* (b a c) d e more)))
   :else a))

(defn infix [& args]
  (infix* args))


(infix 21 / [ 1 + 2 * 3 ])

(defn all-eq? [a b c]
  (infix
   a = b AND b = c AND a = c ))

(all-eq? 1 2 3)

(print (all-eq? 3 3 3))

(= 3 3 4)
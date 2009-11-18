(defn loves [x y]
  (str x " loves " y))
(def log (ref '()))

(defn test-rebind []
  (println (loves "ricky" "lucy"))
  (let [str-orig str]
    (binding [str (fn [& args]
                    (dosync (alter log conj args))
                    (apply str-orig args))]
      (println (loves "fred" "ethel")))))
(test-rebind)
(reverse @log)


(defmacro tryc[ & e]
  `(try ~@e
       (catch Exception a# a#)))


(t (java.io.File ".")) ;; #<ClassCastException java.lang.ClassCastException: java.lang.Class cannot be cast to clojure.lang.IFn>

(defmacro guru [& e]
  `(try ~@e
     (catch Exception a#
       (let [message# (.getMessage a#) 
             st# (seq (.getStackTrace a#))
             unusual# (filter #(let [fn# (.getFileName %)]
                                (and (not (re-matches #".*\.java" fn#))
                                     (not (#{"basic.clj" "core.clj"} fn# ))))  st#)]
         (let [s# (with-out-str
                    (println (class a#) message# "\n--")
                    (pp/pprint (map #(vector (.getFileName %)(.getLineNumber %)(.getClassName %)) unusual#))
                    (println "--\n" (interpret a#)))]
           (println s#)
           s#)))))

(defn interpret [e]
  (cond (and
         (= (class e) java.lang.ClassCastException)
         (= (.getMessage e) "java.lang.Class cannot be cast to clojure.lang.IFn"))
        "Possibly you forgot a dot.\n(java.Class arg) rather than\n(java.Class. arg)\n can cause this.",
        
        :else
        "Confucius he largely mystified by this."))


(print (guru (java.io.File. ".")))
(print (guru (java.io.File ".")))
(print (guru (/ 1 0)))
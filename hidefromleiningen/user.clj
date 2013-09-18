;; Code to run before any repls start
;; You can add an incantation to leiningen which means that this only gets run in 'lein run' tasks, and not packaged into jars and such

(println "beginning of user.clj")


;(set! *print-length* 27) doesn't work because this gets run before thread-local bindings!
(alter-var-root #'*print-length* (constantly 27))
(alter-var-root #'*print-level* (constantly 7))
(alter-var-root #'*warn-on-reflection* (constantly true))


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "->" x#) x#))
                           
(defmacro def-let
  "like let, but binds the expressions globally."
  [bindings & more]
  (let [let-expr (macroexpand `(let ~bindings))
        names-values (partition 2 (second let-expr))
        defs   (map #(cons 'def %) names-values)]
    (concat (list 'do) defs more)))

(import '(java.lang.reflect Modifier Method Constructor))
        
;; A useful function I believe originally due to the mighty Bill Clementson
(defn show
  "list the fields and methods of a class"
  ([x] (show x nil))
  ([x i]
     (let [c (if (class? x) x (class x))
           items (sort
                  (for [m (concat (.getFields c)
                                  (.getMethods c)
                                  (.getConstructors c))]
                    (let [static? (bit-and Modifier/STATIC
                                           (.getModifiers m))
                          method? (instance? Method m)
                          ctor?   (instance? Constructor m)
                          text (if ctor?
                                 (str "(" (apply str (interpose ", " (.getParameterTypes m))) ")")
                                 (str
                                  (if (pos? static?) "static ")
                                  (.getName m) " : "
                                  (if method?
                                    (str (.getReturnType m) " ("
                                         (count (.getParameterTypes m)) ")")
                                    (str (.getType m)))))]
                      [(- static?) method? text (str m) m])))]
       (if i
         (last (nth items i))
         (do (println "=== " c " ===")
             (doseq [[e i] (map list items (iterate inc 0))]
               (printf "[%2d] %s\n" i (nth e 2))))))))


(defmacro require-on-fly 
  "(require-on-fly org.clojure/math.combinatorics \"0.0.4\" clojure.math.combinatorics)"
  [thing version requirename]
  `(do (require 'cemerick.pomegranate)
       (cemerick.pomegranate/add-dependencies 
        :coordinates [['~thing ~version]]
        :repositories (merge cemerick.pomegranate.aether/maven-central  {"clojars" "http://clojars.org/repo"}))
       (require '~requirename)))





(println "end of user.clj (defined user/def-let dbg show and require-on-fly, set! *print-level* and *print-length* and *warn-on-reflection*)")

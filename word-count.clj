(use 'clojure.contrib.duck-streams)
(use 'clojure.contrib.pprint)
(require 'clojure.contrib.str-utils2)


(defn count-line [line]
  (reduce (fn [m s] (assoc m s (inc (get m s 0)))) {}
          (clojure.contrib.str-utils2/split line #"\W+")))

(count-line "you suck you do")

(defn find-widely [filename]
  (apply merge-with +
         (pmap count-line (line-seq (reader filename)))))

(pprint (find-widely "trampoline.clj"))
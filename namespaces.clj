(create-ns 'test)
(count (ns-map 'test))
(count (ns-interns 'test))
(for [x (ns-map 'test)] (format "%s: %s\n" (first x) (rest x)))
(for [x (ns-interns 'test)] (format "%s: %s\n" (first x) (rest x)))

(in-ns 'test)
(clojure.core/defn three [] 3)
(in-ns 'user)

(count (ns-map 'test))
(count (ns-interns 'test))
(for [x (ns-map 'test)] (format "%s: %s\n" (first x) (rest x)))
(for [x (ns-interns 'test)] (format "%s: %s\n" (first x) (rest x)))

(ns-unmap 'test 'three)

(defn seq-xor [& seqs]
  (seq (second
	(reduce (fn [[all ret] x]
		    (if (contains? all x)
		          [all (disj ret x)]
			      [(conj all x) (conj ret x)]))
		[#{} #{}] (mapcat distinct seqs)))))

(seq-xor (keys (ns-map 'test)) (seq-xor (keys (ns-interns 'clojure.core)) (keys(ns-map 'clojure.core))))


(mapcat distinct [(ns-interns 'clojure.core) (ns-map 'clojure.core)])


(keys (ns-map 'clojure.core))

(doc distinct)
;;This catches an agent on the change
(def a (agent 0))
(def output (agent '()))

(defn single-agent [tries]
  (send a (fn[_]0))
  (send output (fn[_]'()))
  (doseq [i (range tries)] 
    (do (let [init [@a @a @a @a @a]]
	  (send a inc)
	  (let [se [@a @a @a @a @a @a @a @a @a @a]]
	    (if (not (apply = (concat init se)))
	      (send output #(cons (str init se) %))
	      (send output #(cons (str init se) %)))))))
  (await output)
  (apply str (reverse @output)))

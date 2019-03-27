
(def dorian ["tonic 2",
             "high 3->2",
             "high 4->3->2",
             "high 5->4->3->2",
             "high 6->5->4->3->2",
             "high 7->6->5->4->3->2",
             "high 1->7->6->5->4->3->2",
             "high 2->1->7->6->5->4->3->2",
             "low 1->2",
             "low 7->1->2",
             "low 6->7->1->2",
             "low 5->6->7->1->2",
             "low 4->5->6->7->1->2",
             "low 3->4->5->6->7->1->2",
             "low 2->3->4->5->6->7->1->2"])






  
(defn tone[] (rand-nth dorian))

;; 6 6 6 6 6 6 6 2 4 6
;; 5 5 5 5 5 5 5 1 3 5
;; 6 6 6 6 6 6 6 7 1 2
;; 1 6 5 3 2 2


;; 2 2 6 6 3 4 3 2
;; 6 1 2 1 6 7 5 6
;; 2 2 2 1 6 6 5 4 3 1
;; 2 6 5 4 3 2 1 2

;; 4 5 6 6 6 4 2 4 5 6 6 6 4 2
;; 3 4 5 5 5 3 1 3 4 5 5 5 3 1
;; 4 5 6 6 6 4 2 4 5 6 6 6 4 2
;; 4 5 6 5 4 3 2



(defn tone-set[]
  (clojure.pprint/pprint (repeatedly 20 #(tone))))



(def small {"a" {:pass 0 :fail 0} "b" {:pass 0 :fail 0}})

(loop [map small]
  (println map)
  (let [k (rand-nth (keys map))]
    (println k (map k) "y/n/x")
    (case (read-line)
      "y" (do (println "yay") (recur (update-in map [k :pass] inc)))
      "n" (do (println "boo") (recur map))
      "x" (print "bye"))))

(binding [*print-dup* true]
  (println small))

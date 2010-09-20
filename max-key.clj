;;meditations on http://clojure101.blogspot.com

(max-key count #{1 2 3} #{1 2})
(min-key count #{1 2 3} #{1 2})

(reverse (sort-by count '({:a 1} {a 2 :b 4} {:c 5})))

(let [m1 (assoc-in  {} [:a] {})
      m2 (assoc-in  m1 [:a :b] [1 2])
      m3 (update-in m2 [:a :b] conj 3)
      m4 (update-in m3 [:a :b] conj 4)
      m5 (update-in m4 [:a] merge {:c [1 2]})
      m6 (update-in m5 [:a] #(merge-with into % {:c [3 4]}))]
  [m1 m2 m3 m4 m5 m6])

(-> {}
    (assoc-in   [:a :b] [1 2])
    (update-in  [:a :b] conj 3)
    (update-in  [:a :b] conj 4)
    (assoc-in   [:a :c] [1 2]))


(merge {:b 3} {:b 2})
(merge-with max {:b 3} {:b 2})
(merge-with min {:b 3} {:b 2})




(into  [3 4] '( 1 2 3))
(into '(3 4) '( 1 2 3))
(into #{3 4} '( 1 2 3))

(list 1 2 3)
(apply list '(1 2 3))
(list* '(1 2 3))
(apply list 1 '(2 3))
(list* 1 '(2 3))


(vector 1 2 3)
(apply vector 1 2 '(3))
(apply vector 1 '(2 3))
(vec 1 '(1 2 3))
[1 2 3]

;;Yikes!
(def one2onemillion (apply vector (range 1000000)))
(time (last one2onemillion))
"Elapsed time: 329.274555 msecs"
999999

(time (peek one2onemillion))
"Elapsed time: 0.318336 msecs"
999999



(frequencies (concat '(1 2 3) #{4 2 1}))
(frequencies (concat {:a 1 :b 3} {:a 1 :b 4}))


(reduce into (replicate 3 [:a :b :c]))
(apply concat (replicate 3 [:a :b :c]))

;; var vs function example

(defn worker []
 (Thread/sleep 2000)
 (prn "not working"))

(defn worker-thread [worker-fn]
  (.start (Thread. #(dotimes [i 10] (worker-fn)))))

(worker-thread worker)   ;;will not pick up redefinition
(worker-thread #'worker) ;;will

(defn worker []
 (Thread/sleep 2000)
 (prn "working"))


;; destructuring def

(defmacro def-let
  "def with binding (def+ [{:keys [a b d]} {:a 1 :b 2 :d 3}])"
  [bindings & more]
  (let [let-expr (macroexpand `(let ~bindings))
        vars (filter #(not (.contains (str %) "__"))
               (map first (partition 2 (second let-expr))))
        def-vars (map (fn [v] `(def ~v ~v)) vars)]
    (concat let-expr def-vars)))

;Example usage:
(def+ [a 1 b 2])
(def+ [{:keys [c d]} {:d 10 :c 20}])
(def+ [z 1 {:keys [aa bb]} {:aa 100 :bb 200}])


(list a b c d aa bb z)


(def-let [r (range 10)
      make-map   (fn [f] (zipmap r (map f r)))
      linearsx10 (make-map #(* 10 %))
      squaresx5  (make-map #(* 5 % %))
      cubes      (make-map #(* % % %))
      mins       (merge-with min squaresx5 cubes linearsx10)
      maxs       (merge-with max squaresx5 cubes linearsx10)]
  (list linearsx10 squaresx5 cubes mins maxs)
  (merge-with + squaresx5 cubes linearsx10)
  (merge-with
   #(if (seq?  %1) (conj %1 %2) (list %1 %2))
   mins maxs squaresx5 cubes linearsx10))


(def+ [r (range 10)
      make-map   (fn [f] (zipmap r (map f r)))
      linearsx10 (make-map #(* 10 %))
      squaresx5  (make-map #(* 5 % %))
      cubes      (make-map #(* % % %))
      mins       (merge-with min squaresx5 cubes linearsx10)
      maxs       (merge-with max squaresx5 cubes linearsx10)]
  (list linearsx10 squaresx5 cubes mins maxs)
  (merge-with + squaresx5 cubes linearsx10)
  (merge-with
   #(if (seq?  %1) (conj %1 %2) (list %1 %2))
   mins maxs squaresx5 cubes linearsx10))





;;debug like

(def+ [linearsx10 (zipmap (range 10) (map #(* 10 %) (range 10)))
       squaresx5  (zipmap (range 10) (map #(* 5 % %) (range 10)))
       cubes      (zipmap (range 10) (map #(* % % %) (range 10)))
       mins       (merge-with min squaresx5 cubes linearsx10)
       maxs       (merge-with max squaresx5 cubes linearsx10)])

(list linearsx10 squaresx5 cubes mins maxs)
(merge-with + squaresx5 cubes linearsx10)
(merge-with #(if (seq?  %1) (conj %1 %2) (list %1 %2)) mins maxs squaresx5 cubes linearsx10)


(range)
(deref (promise))
(let [x (atom 0)] (reset! x {:deeper x}) x)





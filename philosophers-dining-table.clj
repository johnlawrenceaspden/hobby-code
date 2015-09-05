(def number-of-forks 5)

(def initial-table
  (into [] (repeat number-of-forks false)))

initial-table ;[false false false false false]

(defn pick-up-left-fork [n]
  (fn [table]
    (let [fork-number n]
      (if (table fork-number)
        table
        (assoc table fork-number n)))))

(defn pick-up-right-fork [n]
  (fn [table]
    (let [fork-number (mod (inc n) number-of-forks)]
      (if (table fork-number)
        table
        (assoc table fork-number n)))))

(defn put-down-left-fork [n]
  (fn [table]
    (let [fork-number n]
      (if (= n (table fork-number))
        (assoc table fork-number false)
        table ))))

(defn put-down-right-fork [n]
  (fn [table]
    (let [fork-number (mod (inc n) number-of-forks)]
      (if (= n (table fork-number))
        (assoc table fork-number false)
        table ))))

(def pulf0 (pick-up-left-fork 0))
(def purf0 (pick-up-right-fork 0))

(pulf0 initial-table) ; [0 false false false false]
(pulf0 (pulf0 initial-table)) ; [0 false false false false]

(defn perform-actions [table actions]
  (reduce (fn [table action] (action table)) initial-table actions))

(perform-actions initial-table [pulf0]) ; [0 false false false false]
(perform-actions initial-table [pulf0 pulf0]) ; [0 false false false false]
(perform-actions initial-table [pulf0 purf0 pulf0]) ; [0 0 false false false]
(perform-actions initial-table [pulf0 pulf0 purf0 pulf0]) ; [0 0 false false false]

(defn two-forks [table]
  (for [[k v]
        (filter (fn [[k v]] (and (number? k) (= v 2)))
                (frequencies table))]
    k))

(two-forks (pick-up-right-fork 0 (pick-up-left-fork 0 initial-table))) ; (0)



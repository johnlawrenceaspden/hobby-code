;;the philosopher's dining table

(def n 5)

(def chopsticks (map atom (map (fn[_] "down") (range n))))

(defstruct philosopher :status :left-stick :right-stick :has-left-stick :has-right-stick)

(defn make-philosopher [n]
  (struct-map philosopher 
    :status "hungry"
    :left-stick n 
    :right-stick (mod (+ n 1) 5)
    :has-left-stick false
    :has-right-stick false))

(def philosophers (map agent (map make-philosopher (range n))))

(defn state []
  [(map #(list (:status @%)(:has-left-stick @%) (:has-right-stick @%)) philosophers) (map deref chopsticks)])

(defn state-coherent?[]
  (let [philosopher-sticks (mapcat #(list (:has-left-stick @%) (:has-right-stick @%)) philosophers)
	philosopher-stick-count (count (filter #(= % true) philosopher-sticks))
	table-stick-count (count (filter #(= @% "down") chopsticks))]
    (println "philosophers have " philosopher-stick-count "sticks")
    (println table-stick-count "sticks on table")
    (if (=(+ table-stick-count philosopher-stick-count) n)
      true
      false)))

(defn move-stick [ud rl {l :left-stick r :right-stick hl :has-left-stick hr :has-right-stick}]
     (let [stick (if (= rl "left") (nth chopsticks l) (nth chopsticks r))
	   has-stick (if (= rl "left") hl hr)]
       (let [to (if (= ud "up") "up" "down")
	     from (if (= ud "down") "up" "down")
	     should-have-stick (if (= ud "up") false true)
	     phrasal-verb (if (= ud "up") "picked up" "put down")]
	 (if  (= from @stick) 
	   (if (= has-stick should-have-stick)
	     (do
	       (println rl "stick" from)
	       (swap! stick (fn[x] to))
	       (println phrasal-verb rl " stick")
	       true)
	     (do
	       (println "has-stick=" has-stick "should-have-stick" should-have-stick ":fail")
	       false))
	   (do
	     (println rl " stick was " from ": fail")
	     false)))))

(defn update-philosopher [ud rl phil]
  (cond (and (= ud "up") (= rl "right")) (assoc phil :has-right-stick true)
	(and (= ud "up") (= rl "left")) (assoc phil :has-left-stick true)
	(and (= ud "down") (= rl "right")) (assoc phil :has-right-stick false)
	(and (= ud "down") (= rl "left")) (assoc phil :has-left-stick false)))

(defn pick-up-stick  [ud rl philosopher] 
  (if (move-stick       ud rl philosopher)
    (update-philosopher ud rl philosopher)
    philosopher))

(defn pick-up-right-stick   [philosopher] (pick-up-stick "up" "right" philosopher))
(defn pick-up-left-stick    [philosopher] (pick-up-stick "up" "left" philosopher))
(defn put-down-right-stick  [philosopher] (pick-up-stick "down" "right" philosopher))
(defn put-down-left-stick   [philosopher] (pick-up-stick "down" "left" philosopher))

(defn begin-eating [n]
  (let [philosopher (nth philosophers n)]
    (when ( = (:status @philosopher) "hungry")
      (send philosopher pick-up-right-stick)
      (send philosopher pick-up-left-stick)
      (await philosopher))
    (when (and (:has-left-stick @philosopher) (:has-right-stick @philosopher))
      (send philosopher (fn [p] (assoc p :status "happy")))
      (await philosopher))))

(defn finish-eating [n]
  (let [philosopher (nth philosophers n)]
    (when ( = (:status @philosopher) "happy")
      (send philosopher put-down-right-stick)
      (send philosopher put-down-left-stick)
      (await philosopher))
      (send philosopher (fn [p] (assoc p :status "hungry")))
      (await philosopher)))


(defn dinner-party-1 [wait]
  (println (state))
  (send (nth philosophers 0) pick-up-right-stick)
  (when wait (apply await philosophers))
  (println (state))
  (send (nth philosophers 0) pick-up-left-stick)
  (when wait (apply await philosophers))
  (println (state))
  (send (nth philosophers 0) put-down-left-stick)
  (when wait (apply await philosophers))
  (println (state))
  (send (nth philosophers 0) put-down-right-stick)
  (when wait (apply await philosophers))
  (println (state)))

(defn dinner-party-2 []
  (let [p (rand-int n)]
    (if (=(rand-int 2) 0)
      (do
	(println "philosopher" p "begins eating")
	(begin-eating p))
      (do
	(println "philosopher" p "finishes eating")
	(finish-eating p)))))
  

(defn go [fn n] 
  (if (> n 0) 
    (do
      (println "loop" n) 
      (fn)
      (println (state))
      (if (state-coherent?)
	(recur fn (- n 1))
	(println ("!!!!!!BROKEN!!!!!!"))))
      (println "finished normally")))
    
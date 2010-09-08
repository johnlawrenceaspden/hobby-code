;; How do genes spread through a population? Will brown eyes drive out blue?

(def population (concat (repeat 3 "BB") (repeat 0 "Bb") (repeat 97 "bb")))

(defn random-mating [pop]
     (let [mum (rand-nth pop)
           dad (rand-nth pop)]
       (let [m (rand-nth mum)
             p (rand-nth dad)]
         (let [baby (if (= 0 (rand 2)) (str m p) (str p m))]
           (cons baby pop)))))

(defn random-death [pop]
  (drop 1 (shuffle pop)))

(random-death population)

(defn pop->numbers [pop]
  (let [pop (frequencies pop)]
    [(get pop "BB" 0), (+ (get pop "Bb" 0) (get pop "Bb" 0)), (get pop "bb" 0)]))

(defn numbers->percentstr [numbers]
  (let [total (reduce + numbers)]
    (apply format "%d|%.2f:%.2f:%.2f" total (map #(/ % total 1.0) numbers))))

(def showpop (comp numbers->frequencies pop->numbers))

(defn random-event [pop]
  (let [r (rand)]
    (cond (< r (/ 100 (count pop))) (random-mating pop)
          :else                     (random-death  pop))))
        


(numbers->frequencies (pop->numbers population))

(def history (iterate random-event population))
(def stats   (map showpop history))


(map #(nth stats %) (range 1 10000 1000))
(nth stats 200)
(nth stats 300)




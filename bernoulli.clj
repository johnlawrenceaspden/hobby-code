;; A Bernoulli economy is one where everyone swaps small amounts of money at
;; random. I'm interested in seeing what the wealth distribution in such an
;; economy looks like.

;; We'll make a vector of ten people with ten pounds each

(def N 10000)

(defn transact [a b pop]
  (dosync
   (when ( > @(nth pop b) 0)
     (alter (nth pop a) inc)
     (alter (nth pop b) dec))))

(defn run [its pop]
  (let [N (count pop)]
    (dotimes [i its]
      (transact (rand-int N) (rand-int N) pop))
    (map deref pop)))


(use 'simple-plotter)

(defn display [f]
  (apply plot (first f))
    (doseq [[a b] (rest f)]
      (draw-to a b)))

(do
  (create-window "bernoulli" 1024 768 white black 0 100 0 1000)
  (def people (apply vector (for [i (range N)] (ref 10))))
  (loop [freqs '()]
    (let [freqs (cons (sort (frequencies (run 10000 people))) freqs)]
      (let [df (reverse (take 25 freqs))]
        (cls)
        (doseq [i (range (count df))]
          (ink (java.awt.Color. (+ 5 (* 10 i)) 0 0))
          (display (nth df i))))
      (recur freqs))))


;; (reduce (fn [a b] (+ a @b)) 0 people)

;; (use '(incanter core charts stats))
;; (view (histogram (map deref people) :nbins 100))


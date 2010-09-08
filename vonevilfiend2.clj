;; Dr von Evilfiend, being a conscientious psychotic overlord, has recorded the
;; permutation of the identity cards in the postboxes, and on the fourth day
;; of the game, he finds that the signature is:

(def signature '(48 20 15 7 5 4 1))

(reduce + signature) ;; 100

;; He expects that, if the prisoners have got their strategy right,
;; forty eight prisoners will need to examine 48 letters,
;; twenty will need to examine 20, and so on.

;; He thinks that it will take about ten seconds to open a box and move on to
;; the next one.

(/ (* 10.0 (reduce + (map #(* % %) signature))) 60 60) ;; 8.38888888888889

;; And he therefore feels that the prisoners will need about 8 hours to win.
;; He is preparing to take certain favourite textbooks for a walk in the woods.

;; But the prisoners have found that the guards can be distracted long enough
;; for each prisoner to swap the contents of any two boxes.

(defn split [n]
  (if (= (mod n 2) 0) [(/ n 2), (/ n 2)]
      [(/ (inc n) 2), (/ (dec n) 2)]))

(map split (range 10))

(defn frig [signature]
  (let [[head & tail] (reverse (sort signature))]
    (if (= head 1) signature
        (concat (split head) tail))))

(take 10 (iterate frig signature))







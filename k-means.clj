;; K-means

;; K-means is a Clustering Algorithm.

;; We use it when we have some data, and we want to split the data into separate categories.

;; For instance, an early biologist, let's call him Adam, might measure all
;; sorts of things about the living objects he encounters in the world.

;; (black, feathers, flies)
;; (green, tasty)
;; (green, slithers, poisonous)
;; ... and so on

;; After he collects enough data, he might go looking for structure in it.

;; Uninformed by theory, he might nevertheless notice that many things that do
;; not move are green, and that many things that are not green move.

;; He might name these two obvious groups the Animals and the Plants.

;; Further analysis of the data might split the Plants into Trees and Flowers,
;; and the Animals into Mammals, Birds, and Fish.

;; Theoretically, this process could continue further, extracting 'natural
;; categories' from the observed structure of the data.

;; Let's consider a very simple clustering situation. We have some numbers,
;; and we'd like to see if they form groups.

;; Suppose we want to cluster the points 2 3 5 6 10 11 100 101 102
(def data '(2 3 5 6 10 11 100 101 102))
;; You may be able to see some natural groupings in this data.

;; It's easy enough to say how far one number is from another
(defn distance[a b] = (if (< a b) (- b a) (- a b)))

(distance 0 10)                         ; 10
(distance 5 -2)                         ; 7


;; To do K-means, we need to start with some guesses about where the clusters are.
;; They don't have to be terribly good guesses.
(def guessed-means '(0 10))

;; Given a particular point, we want to know which of our means is closest
(defn closest [point means distance]
  (second (first (sort-by first (for [m means] [(distance point m) m])))))

;; From our actual data, 2 is closest to the guess of 0, and 100 is closest to the guess of 10
(closest 2   guessed-means distance) ; 0
(closest 100 guessed-means distance) ; 10

;; So we can talk about the group of all points for which 0 is the best guess
;; and the group of all points for which 10 is the best guess.
(defn point-groups [means data distance]
  (group-by #(closest % means distance) data))

(point-groups guessed-means data distance) ; {0 [2 3 5], 10 [6 10 11 100 101 102]}

;; We can take an average of a group of points
(defn average [& list] (/ (reduce + list) (count list)))

(average 6 10 11 100 101 102) ; 55

;; So now we can take the average of each group, and use it as a new guess for
;; where the clusters are.
(defn new-means [average point-groups]
  (for [[m pts] point-groups]
    (apply average pts)))

(new-means average (point-groups guessed-means data distance)) ; (10/3 55)

;; So if we know we've got a particular set of points, and a particular idea of
;; distance, and a way of averaging things, that gives us a way of making a new
;; list of guesses from our original list of guesses
(defn iterate-means [data distance average]
  (fn [means] (new-means average (point-groups means data distance))))

((iterate-means data distance average) '(0 10)) ; (10/3 55)

;; and of course we can use that as a new guess, and improve it again.
((iterate-means data distance average) '(10/3 55)) ; (37/6 101)


;; We can do this repeatedly until it settles down.
(iterate (iterate-means data distance average) '(0 10)) ; ((0 10) (10/3 55) (37/6 101) (37/6 101) .....

;; K-means with two means seems to be trying to tell us that we've got a group
;; centered around 6 and another centred around 101

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nothing we said above limits us to only having two guesses
(iterate (iterate-means data distance average) '(1 5 10)) ; ((1 5 10) (5/2 11/2 324/5) (5/2 8 101) (10/3 9 101) (4 21/2 101) (4 21/2 101)
(iterate (iterate-means data distance average) '(0 1 2 3 4)) ; ((0 1 2 3 4) (2 3 335/7) (2 7 101) (5/2 8 101) (10/3 9 101) (4 21/2 101) ..

;; Notice how trial means get thrown away if they don't best explain any point.

;; We have to be careful not to start off with too many means, or we just get our data back:
(iterate (iterate-means data distance average) (range 10)) ; ((0 1 2 3 4 5 6 7 8 9) (2 3 5 6 324/5) (2 3 5 9 101) (2 3 11/2 21/2 101) (2 3 11/2 21/2 101) ....

;; But even here, because means have been thrown away, we don't just get a cluster for every point.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Similarly, nothing limited us to numbers. We can use anything where we can define a distance, and a method of averaging:

;; One of the easiest things to do this for would be vectors

(defn vec-distance [a b] (reduce + (map #(* % %) (map - a b))))
(defn vec-average  [& l]  (map #(/ % (count l)) (apply map + l)))

(vec-distance [1 2 3][5 6 7]) ; 48
(vec-average  [1 2 3][5 6 7]) ; (3 4 5)


;; But we can be a little more ambitious: Here are the results of Adam's original survey
(def eden-objects '((python red yellow slithers scales)
                    (trout brown swims tasty)
                    (lion yellow walks runs fast)
                    (lamb white cute tasty walks runs)
                    (crab orange tasty swims painful)
                    (eve sexy walks runs)
                    (brocolli green tasty)
                    (oranges orange tasty)
                    (sprouts green tasty)))

;; Which can be transformed into a map of maps without too much trouble
(def eden-map
     (into {}
           (map
            (fn [ [name & features]] [name (apply hash-map (interleave features (repeat 1)))])
            eden-objects)))

(eden-map 'crab) ; {tasty 1, orange 1, painful 1, swims 1}

;; We want to be able to extract a set of characteristics with its name
(defn eden-entry [s] [s (eden-map s)])

(eden-entry 'crab) ; [crab {tasty 1, orange 1, painful 1, swims 1}]

;; Now we need distance and averaging functions for the maps
(defn map-distance [a b] (reduce + (map #(* % %) (vals (merge-with - (second a) (second b))))))
(defn map-average  [& l] ['composite (into {} (map (fn [[a b]] [a (/ b (count l))]) (apply merge-with + (map second l))))])

;; Which are very similar to the ones for vectors
(map-distance (eden-entry 'eve) (eden-entry 'lion)) ; 3
(map-average (eden-entry 'eve) (eden-entry 'lion) (eden-entry 'brocolli)) ; [composite {walks 2/3, yellow 1/3, runs 2/3, tasty 1/3, fast 1/3, green 1/3, sexy 1/3}]

(defn eden-means [archetypes its]
  (nth (iterate (iterate-means eden-map map-distance map-average) archetypes) its))

(defn eden-groups [archetypes its]
  (map #(map first %) (vals
       (point-groups (eden-means archetypes its) eden-map map-distance))))



(eden-groups (list (eden-entry 'eve) (eden-entry 'lion)) 3) ; ((lamb sprouts trout brocolli eve crab oranges) (python lion))
(eden-groups (list (eden-entry 'crab) (eden-entry 'lion)) 3) ; ((lamb python eve lion) (sprouts trout brocolli crab oranges))

(eden-groups (list (eden-entry 'sprouts) (eden-entry 'brocolli)) 3) ; ((lamb sprouts python trout brocolli eve lion crab oranges))
(eden-groups (list (eden-entry 'python) (eden-entry 'lion)) 10) ; ((lamb eve lion) (sprouts python trout brocolli crab oranges))

(eden-groups (list (eden-entry 'eve) (eden-entry 'lion)) 5) ; ((lamb sprouts trout brocolli eve crab oranges) (python lion))


(eden-groups (list (eden-entry 'brocolli) (eden-entry 'eve) (eden-entry 'lion)) 0) ; ((lamb eve) (sprouts python trout brocolli crab oranges) (lion))
(eden-groups (list (eden-entry 'brocolli) (eden-entry 'eve) (eden-entry 'lion)) 1) ; ((lamb eve) (sprouts python trout brocolli crab oranges) (lion))
(eden-groups (list (eden-entry 'brocolli) (eden-entry 'eve) (eden-entry 'lion)) 2) ; ((lamb eve) (sprouts python trout brocolli crab oranges) (lion))
(eden-groups (list (eden-entry 'brocolli) (eden-entry 'eve) (eden-entry 'lion)) 3) ; ((lamb eve) (sprouts python trout brocolli crab oranges) (lion))
(eden-groups (list (eden-entry 'brocolli) (eden-entry 'eve) (eden-entry 'lion)) 4) ; ((lamb eve) (sprouts python trout brocolli crab oranges) (lion))
(eden-groups (list (eden-entry 'brocolli) (eden-entry 'trout) (eden-entry 'lion)) 5) ; ((lamb eve) (sprouts python trout brocolli crab oranges) (lion))






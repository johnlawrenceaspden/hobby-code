;; K-means

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; categories' from the observed structure of the data, without any theory about
;; how the various properties come about

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's consider a very simple clustering situation. We have some numbers,
;; and we'd like to see if they form groups.

;; Suppose we want to cluster the points 2 3 5 6 10 11 100 101 102
(def data '(2 3 5 6 10 11 100 101 102))
;; You may be able to see some natural groupings in this data.

;; It's easy enough to say how far one number is from another
(defn distance[a b] = (if (< a b) (- b a) (- a b)))

;; To do K-means, we need to start with some guesses about where the clusters are.
;; They don't have to be terribly good guesses.
(def guessed-means '(0 10))

;; Given a particular point, we want to know which of our means is closest
(defn closest [point means distance]
  (first (sort-by #(distance % point) means)))

;; In our little dataset, 2 is closest to the guess of 0, and 100 is closest to the guess of 10
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

;; So we can take the average of each group, and use it as a new guess for where
;; the clusters are. If a mean doesn't have a group, then we'll leave it where
;; it is.
(defn new-means [average point-groups old-means]
  (for [o old-means]
    (if (contains? point-groups o)
      (apply average (get point-groups o)) o)))

(new-means average (point-groups guessed-means data distance) guessed-means) ; (10/3 55)

;; So if we know we've got a particular set of points, and a particular idea of
;; distance, and a way of averaging things, that gives us a way of making a new
;; list of guesses from our original list of guesses
(defn iterate-means [data distance average]
  (fn [means] (new-means average (point-groups means data distance) means)))

((iterate-means data distance average) '(0 10)) ; (10/3 55)

;; and of course we can use that as a new guess, and improve it again.
((iterate-means data distance average) '(10/3 55)) ; (37/6 101)

;; We can do this repeatedly until it settles down.
(iterate (iterate-means data distance average) '(0 10)) ; ((0 10) (10/3 55) (37/6 101) (37/6 101) .....)

;; K-means with two means seems to be trying to tell us that we've got a group
;; centered around 6 and another centred around 101

;; These groups are:
(defn groups [data distance means]
  (vals (point-groups means data distance)))

(groups data distance '(37/6 101)) ; ([2 3 5 6 10 11] [100 101 102])

;; Ideally we'd like to iterate until the groups stop changing.
;; I described a function for doing this in a previous post:
(defn take-while-unstable 
  ([sq] (lazy-seq (if-let [sq (seq sq)]
                    (cons (first sq) (take-while-unstable (rest sq) (first sq))))))
  ([sq last] (lazy-seq (if-let [sq (seq sq)]
                         (if (= (first sq) last) '() (take-while-unstable sq))))))

(take-while-unstable '(1 2 3 4 5 6 7 7 7 7)) ; (1 2 3 4 5 6 7)

(take-while-unstable (map #(groups data distance %) (iterate (iterate-means data distance average) '(0 10))))
                                        ; (([2 3 5] [6 10 11 100 101 102])
                                        ;  ([2 3 5 6 10 11] [100 101 102]))

;; Shows that our first guesses group 2,3 and 5 (nearer to 0 than 10) vs all the rest.
;; K-means modifies that instantly to separate out the large group of three.


;; We can make a function, which takes our data, notion of distance, and notion of average,
;; and gives us back a function which, for a given set of initial guesses at the means,
;; shows us how the group memberships change.
(defn k-groups [data distance average]
  (fn [guesses]
    (take-while-unstable
     (map #(groups data distance %)
          (iterate (iterate-means data distance average) guesses)))))

(def grouper (k-groups data distance average))

(grouper '(0 10))
                                        ; (([2 3 5] [6 10 11 100 101 102])
                                        ;  
                                        ;  ([2 3 5 6 10 11] [100 101 102]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nothing we said above limits us to only having two guesses
(grouper '(1 2 3))
                                        ; (([2] [3 5 6 10 11 100 101 102])
                                        ;  ([2 3 5 6 10 11] [100 101 102])
                                        ;  ([2 3] [5 6 10 11] [100 101 102])
                                        ;  ([2 3 5] [6 10 11] [100 101 102])
                                        ;
                                        ;  ([2 3 5 6] [10 11] [100 101 102]))


;; The more means we start with, the more detailed our clustering.
(grouper '(0 1 2 3 4))
                                        ; (([2] [3] [5 6 10 11 100 101 102])
                                        ;  ([2] [3 5 6 10 11] [100 101 102])
                                        ;  ([2 3] [5 6 10 11] [100 101 102])
                                        ;  ([2 3 5] [6 10 11] [100 101 102])
                                        ;  ([2] [3 5 6] [10 11] [100 101 102])
                                        ;
                                        ;  ([2 3] [5 6] [10 11] [100 101 102]))

;; We have to be careful not to start off with too many means, or we just get our data back:
(grouper (range 200)) ; (([2] [3] [100] [5] [101] [6] [102] [10] [11]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generalizing to Other Spaces

;; In fact, nothing we said above depends on our inputs being numbers

;; We can use any data where we can define a distance, and a method of averaging:

;; One of the easiest things to do this for would be vectors:

(defn vec-distance [a b] (reduce + (map #(* % %) (map - a b))))
(defn vec-average  [& l]  (map #(/ % (count l)) (apply map + l)))

(vec-distance [1 2 3][5 6 7]) ; 48
(vec-average  [1 2 3][5 6 7]) ; (3 4 5)

;; Here's a little set of vectors
(def vector-data '( [1 2 3] [3 2 1] [100 200 300] [300 200 100] [50 50 50]))

;; And choosing three guesses in a fairly simple-minded manner, we can see how the algorithm
;; divides them into three different groups.
((k-groups vector-data vec-distance vec-average) '([1 1 1] [2 2 2] [3 3 3]))
                                        ; (([[1 2 3] [3 2 1]] [[100 200 300] [300 200 100] [50 50 50]])

                                        ;  ([[1 2 3] [3 2 1] [50 50 50]]
                                        ;   [[100 200 300] [300 200 100]])

                                        ;  ([[1 2 3] [3 2 1]]
                                        ;   [[100 200 300] [300 200 100]]
                                        ;   [[50 50 50]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pedantic Footnote

;; Note that the algorithm as described above isn't quite the classic K-means.
;; I don't think the difference is terribly important, and I thought it would complicate the explanation to deal with it.

;; In the usual K-means, if you have two identical means, then you're only supposed to update one of them.

;; Here our two identical guesses are both getting updated
(new-means average (point-groups '(0 0) '(0 1 2 3 4) distance) '(0 0)) ; (2 2)

;; Our update function:
(defn new-means [average point-groups old-means]
  (for [o old-means]
    (if (contains? point-groups o)
      (apply average (get point-groups o)) o)))

;; Needs to be changed so that if there are two identical means only one of them will be changed:

(defn update-seq [sq f]
  (let [freqs (frequencies sq)]
    (apply concat
     (for [[k v] freqs]
       (if (= v 1) (list (f k))
           (cons (f k) (repeat (dec v) k)))))))


(defn new-means [average point-groups old-means]
  (update-seq old-means (fn[o]
                          (if (contains? point-groups o)
                            (apply average (get point-groups o)) o))))

;; Now only one will get updated at once
(new-means average (point-groups '(0 0) '(0 1 2 3 4) distance) '(0 0)) ; (2 0)


;; Now we don't lose groups when the means get aliased.
((k-groups '(0 1 2 3 4) distance average) '(0 1)) ; (([0] [1 2 3 4]) ([0 1] [2 3 4]))
((k-groups '(0 1 2 3 4) distance average) '(0 0)) ; (([0 1 2 3 4]) ([0] [1 2 3 4]) ([0 1] [2 3 4]))

((k-groups vector-data vec-distance vec-average) '([1 1 1] [1 1 1] [1 1 1])) ;
                                        ; (([[1 2 3] [3 2 1] [100 200 300] [300 200 100] [50 50 50]])
                                        ;  ([[1 2 3] [3 2 1]] [[100 200 300] [300 200 100] [50 50 50]])
                                        ;  ([[1 2 3] [3 2 1] [50 50 50]] [[100 200 300] [300 200 100]])
                                        ;  ([[1 2 3] [3 2 1]] [[100 200 300] [300 200 100]] [[50 50 50]]))

;; Although it's still possible that a mean never acquires any points, so we can still get out fewer groups than means.
((k-groups '(0 1 2 3 4) distance average) '(0 5 10)) ; (([0 1 2] [3 4]))


    














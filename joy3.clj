(def bearings [{:x  0 :y  1}
               {:x  1 :y  0}
               {:x  0 :y -1}
               {:x -1 :y  0}])

(defn forward [x y bearing]
  [(+ x (:x (bearings bearing)))
   (+ y (:y (bearings bearing)))])

(forward 5 5 0) ; [5 6]
(forward 5 5 1) ; [6 5]
(forward 5 5 2) ; [5 4]
(forward 5 5 3) ; [4 5]

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))})

(bot 0 0 0) ; {:coords [0 0], :bearing :north, :forward #<user$bot$fn__11491 user$bot$fn__11491@193d548>}

(:coords (bot 0 0 0)) ; [0 0]
(:bearing (bot 0 0 0)) ; :north
((:forward (bot 0 0 0))) ; {:coords [0 1], :bearing :north, :forward #<user$bot$fn__11552 user$bot$fn__11552@164d42f>}

(map :coords (iterate #( (:forward %)) (bot 0 0 0))) ; ([0 0] [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] [0 8] [0 9] [0 10] [0 11] [0 12] [0 13] [0 14] [0 15] [0 16] [0 17] [0 18] [0 19] [0 20] [0 21] [0 22] [0 23] [0 24] [0 25] [0 26] [0 27] [0 28] [0 29] [0 30] [0 31] [0 32] [0 33] [0 34] [0 35] [0 36] [0 37] [0 38] [0 39] [0 40] [0 41] [0 42] [0 43] [0 44] [0 45] [0 46] [0 47] [0 48] [0 49] [0 50] [0 51] [0 52] [0 53] [0 54] [0 55] [0 56] [0 57] [0 58] [0 59] [0 60] [0 61] [0 62] [0 63] [0 64] [0 65] [0 66] [0 67] [0 68] [0 69] [0 70] [0 71] [0 72] [0 73] [0 74] [0 75] [0 76] [0 77] [0 78] [0 79] [0 80] [0 81] [0 82] [0 83] [0 84] [0 85] [0 86] [0 87] [0 88] [0 89] [0 90] [0 91] [0 92] [0 93] [0 94] [0 95] [0 96] [0 97] [0 98] [0 99] [0 100] [0 101] [0 102] ...)


(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))
   :turn-left  (fn [] (bot x y (mod (dec bearing-num) 4)))
   :turn-right (fn [] (bot x y (mod (inc bearing-num) 4)))})

(:coords ((:forward ((:turn-right ((:forward (bot 0 0 0)))))))) ; [1 1]

((juxt :coords :bearing) (reduce (fn [bot action] ((action bot))) (bot 0 0 0) '(:forward :turn-right :forward))) ; [[1 1] :east]

;; x=(bot(0, 0, 0).forward().forward().turn-right())
;; x.coords()
;; x.bearing()


(defn mirror-bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (mirror-bot (- x (:x (bearings bearing-num)))
                               (- y (:y (bearings bearing-num)))
                               bearing-num))
   :turn-left  (fn [] (mirror-bot x y (mod (inc bearing-num) 4)))
   :turn-right (fn [] (mirror-bot x y (mod (dec bearing-num) 4)))
   :mirror (fn[] (bot x y bearing-num))})

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))
   :turn-left  (fn [] (bot x y (mod (dec bearing-num) 4)))
   :turn-right (fn [] (bot x y (mod (inc bearing-num) 4)))
   :mirror (fn[] (mirror-bot x y bearing-num))})

(defn bot-actions [bot alist]
  (reductions (fn [bot action] ((action bot))) bot alist))

(def report (juxt :coords :bearing))

(map report (bot-actions (bot 0 0 0)        '(:forward :turn-right :forward :mirror :turn-left))) ; ([[0 0] :north] [[0 1] :north] [[0 1] :east] [[1 1] :east] [[1 1] :east] [[1 1] :south])
(map report (bot-actions (mirror-bot 0 0 0) '(:forward :turn-right :forward :mirror :turn-left))) ; ([[0 0] :north] [[0 -1] :north] [[0 -1] :west] [[1 -1] :west] [[1 -1] :west] [[1 -1] :south])

((:turn-right (mirror-bot 0 0 0)))





















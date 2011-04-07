;; Here's a set of data which we wish to model
(def points #{-5 -6 1 1.1  5})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First, some plotting routines that will come in handy
(use 'simple-plotter)


(defn plot-fn[f]
  (ink black)
  (plot -7 (f -7))
  (doseq [x (range -7 7 0.01)]
    (draw-to x (f x)))
  (ink lightgray)
  (axes))

(defn mark-points [points]
  (ink red)
  (doseq [p points] 
    (line p -0.1 p 1.0)))

(defn log-likelihood-line [l]
  (ink green)
  (let [a (+ (/ l 10) 1)]
    (line -7 a +7 a)))

(defn make-new-window[name]
  (create-window name 800 400 black white -7.0 7.0 -0.1 1.0)
  (mark-points points))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the integral over R of e^{-x^2} is sqrt{tau}
;; the inverse of which is roughly 40%
(def gaussian-constant (/ (Math/sqrt (* 2 Math/PI)))) ; 0.3989422804014327

;; This leads us to the gaussian with centre mu and breadth sigma
;; which is simply e^{-x^2} scaled and shifted,
;; then divided by sigma to make its area equal to one.
(defn gaussian [sigma, mu]
  (let [C (/ gaussian-constant sigma)]
        (fn [x]
          (let [s (/ (- x mu) sigma)
                e  (/ (* s s) 2 )]
          (* C (Math/exp (- e)))))))

;; Here's a standard unscaled gaussian
;; (def g (gaussian 1 0))

;; What happens if we make a distribution from two gaussians with known sigma=1,
;; but unknown means m1 and m2?
(defn double-gaussian[m1,m2]
  (let [g1 (gaussian 1 m1)
        g2 (gaussian 1 m2)]
    (fn [x] (/(+ (g1 x) (g2 x)) 2))))

;; We can calculate the likelihood of our data given our particular set of data points
(defn likelihood[g]
  (reduce * (map g points)))

;; Because likelihoods vary so wildly, we'd rather work in logs, which is a mathematician's way
;; of saying 'don't tell me the details, just tell me how many big it is'.

(defn log-likelihood[g]
  (reduce + (map #(Math/log (g %)) points)))


;; This function will give us the log-likelihood of a double gaussian with means m1 and m2
;; and as a side effect, plot it, and mark the log-likelihood on the graph with a green line
(defn try-ms [m1 m2]
  (let [g (double-gaussian m1 m2)]
    (plot-fn g)
    (let [ll (log-likelihood g)]
      (log-likelihood-line ll)
      ll)))

;; We can just calculate without drawing
(defn lldg [m1 m2]
  (let [g (double-gaussian m1 m2)]
    (log-likelihood g)))
;; It looks like moving either mean to the right is an improvement,
;; but how much?

(defn lldg-change [m1 m2 d1 d2]
  (- (lldg (+ m1 d1) (+ m2 d2))
     (lldg m1 m2)))


(defn length-squared [dx,dy] = (+ (* dx dx) (* dy dy)))
(defn length [dx,dy] (Math/sqrt (length-squared dx dy)))



;; So we can make a function to turn vectors into unit vectors in the same direction
(defn unit-vector [[dx dy]]
  (let [l (length dx dy)]
    [(/ dx l) (/ dy l)]))


;; And now we can wrap that up to make a direction finding function

(defn unit-direction-to-go-in [m1 m2]
  (let [dx (lldg-change m1 m2 0.0001 0)
        dy (lldg-change m1 m2 0 0.0001)]
    (unit-vector [dx dy])))


(defn change-for-small-step [m1 m2 step]
  (let [[dx dy] (unit-direction-to-go-in m1 m2)]
    (-
     (lldg (+ m1 (* step dx)) (+ m2 (* step dy)))
     (lldg m1 m2))))


(defn long-jump [[m1 m2]]
  (let [direction (unit-direction-to-go-in m1 m2)
        step-size 0.001
        tiny-shift (map #(* step-size %) direction)
        original [m1 m2]
        first-move (map + tiny-shift original)
        second-move (map + tiny-shift first-move)
        first-change (- (apply lldg first-move) (apply lldg original))
        second-change (- (apply lldg second-move) (apply lldg first-move))
        change-in-change (- first-change second-change)
        steps-to-take (/ first-change change-in-change)
        large-shift (map #(* steps-to-take step-size %) direction)
        [dx dy] large-shift]
    [(+ m1 dx) (+ m2 dy)]))


(clojure.pprint/pprint (take 20 (iterate long-jump [0 1])))
([0 1]
 [-6.6733129960658975 3.6207992689495203]
 [-5.810889668999718 2.237942650857803]
 [-5.538379815414535 2.407153375687623]
 [-5.510431852224534 2.362817724070082]
 [-5.501502636547292 2.3677188093770174]
 [-5.500711713595741 2.366818683035126]
 [-5.500545464271322 2.3667425523043804]
 [-5.500529170983446 2.3667363428562127]
 [-5.50052789753418 2.3667358657897934]
 [-5.500527800167348 2.3667358293623035]
 [-5.500527792736444 2.3667358265824903]
 [-5.5005277921744815 2.366735826372268]
 [-5.500527792131374 2.3667358263561424]
 [-5.500527792116483 2.366735826350572]
 [-5.5005277921235365 2.3667358263532106]
 [-5.500527792119618 2.3667358263517446]
 [-5.50052779211805 2.3667358263511584]
 [-5.500527792122753 2.3667358263529175]
 [-5.500527792120402 2.3667358263520377])
nil


;; Our means stopped moving on about the twelfth step

;; But in fact, the log-likelihood got about as good as it's going to get on the sixth step.
(make-new-window "twin gaussians") ; nil

(defn its (iterate long-jump [0 1]))

(cls)
(mark-points points)
(apply try-ms (nth (iterate long-jump [0 0.01]) 20))

;; Notice how the means tend to end up at the average values of clusters of points.

;; We have almost reinvented K-means, but not quite.

;; With three points, rather than one cluster of one point and one cluster of two, the two
;; gaussians have decided to split the middle point between them.

;; This algorithm is known as soft K-means


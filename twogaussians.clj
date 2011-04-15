;; Here's a set of data which we wish to model
(def points #{1 3 5})

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

;; Let's have a look at some examples
(do
  (make-new-window "twin gaussians")
  (doseq [ i (range 7)]
    (plot-fn (double-gaussian 0 i))))
;; We've marked our three data points on the graph in red. Which of the double-gaussians is
;; most likely to have produced the three points?


;; We can calculate the likelihood of our data given our particular set of data points
(defn likelihood[g]
  (reduce * (map g points)))

(likelihood (double-gaussian 0 0)) ; 1.5943246622599484E-9
(likelihood (double-gaussian 1 4)) ; 0.0036129047619591877

;; Because likelihoods vary so wildly, we'd rather work in logs, which is a mathematician's way
;; of saying 'don't tell me the details, just tell me how many big it is'.

(defn log-likelihood[g]
  (reduce + (map #(Math/log (g %)) points)))

;; On our smallness scale, 1.5x10^-9 is about -20, 0.003 is about -5.
;; Less negative means more likely.

(log-likelihood (double-gaussian 0 0)) ; -20.256815599614015
(log-likelihood (double-gaussian 1 4)) ; -5.623243186987146



;; This function will give us the log-likelihood of a double gaussian with means m1 and m2
;; and as a side effect, plot it, and mark the log-likelihood on the graph with a green line
(defn try-ms [m1 m2]
  (let [g (double-gaussian m1 m2)]
    (plot-fn g)
    (let [ll (log-likelihood g)]
      (log-likelihood-line ll)
      ll)))

(make-new-window "twin gaussians")

;; Means 0 & 0 are fantastically unlikely to generate 1, 3 and 5
(try-ms 0 0) ; -20.256815599614015

;; Five is particularly unlikely, being 5 standard deviations away from the mean
;; So we should shift one of the means over to the right

(try-ms 0 1) ; -14.272242677972605

;; That's a bit better, let's keep doing that

(try-ms 0 2) ; -9.624624626443204

;; At an unlikeliness factor of 9, a green line has appeared on our graph
;; (earlier ones were too unlikely to show)

;; Let's keep at it.

(try-ms 0 3) ; -7.1237685823922785

;; even better, let's also shift the left hump in a bit closer to the data

(try-ms 1 3) ; -6.579925434070178
(try-ms 1 4) ; -5.623243186987146

;; better and better, what about having the means at 1 and 5, which are our outside points?

(try-ms 1 5) ; -6.142439147988117

;; Apparently that's less likely to generate our data.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OK, so now I'm stuck and don't know how to make it better
;; time to use science

;; Let's take our best shot so far:

(make-new-window "twin gaussians")

(try-ms 1 4) ; -5.623243186987147

;; and check the effect of moving to 1.1 4
(try-ms 1.1 4) ; -5.589393942341673

;; a slight improvement

;; We can just calculate without drawing
(defn lldg [m1 m2]
  (let [g (double-gaussian m1 m2)]
    (log-likelihood g)))

(lldg 1.1 4) ; -5.589393942341673
(lldg 1 4.1) ; -5.6161904762279695

;; It looks like moving either mean to the right is an improvement,
;; but how much?

(defn lldg-change [m1 m2 d1 d2]
  (- (lldg (+ m1 d1) (+ m2 d2))
     (lldg m1 m2)))

(lldg-change 1 4 0.1 0) ; 0.033849244645473675
(lldg-change 1 4 0 0.1) ; 0.007052710759177572

;; We see that if we change m1 by 0.1 then the likelihood
;; goes up by 0.03
;; And if we change m2 by the same amount, then it goes up
;; by only 0.007

;; For best improvement with least movement, we should go in the
;; 0.03,0.007 direction

(lldg-change 1 4 0.03 0.007) ; 0.011823177677271701

;; Here are some steps in that direction
(try-ms 1.03 4.007) ; -5.6114200093098745
(try-ms 1.06 4.014) ; -5.600057744971213
(try-ms 1.09 4.021) ; -5.589155679777338
(try-ms 1.12 4.028) ; -5.578714645178728
(try-ms 1.15 4.035) ; -5.568737060653577
(try-ms 1.18 4.042) ; -5.559226963111431

;; we're getting steady small improvements, so let's try larger jumps

(try-ms (+ 1 (* 0.03 10)) (+ 4 (* 0.007 10))) ; -5.5259992902235755
(try-ms (+ 1 (* 0.03 20)) (+ 4 (* 0.007 20))) ; -5.481165444654136

;; As we move both means to the right in the proportions given, the likelihood
;; gets consistently better.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's just pause to think what the log-likelihood means:
;; (a lot of people get confused by logs, including me)

;; Here are the powers of e, rounded to the nearest integer
(map #(Math/round (Math/exp %)) '(1 2 3 4 5 6 7 8 9 10 11 12))
;; (3 7 20 55 148 403 1097 2981 8103 22026 59874 162755)

;; e^-5.481 ~ 1/275 0.4%, or even more roughly 6 nats is 403

;; We might think of this likelihood  as meaning that
;; a double gaussian with both sigmas = 1 and means 1 and 4
;; has about a 1/275 chance of generating its first point in the range 0.5 to 1.5, its second in the range 2.5 to 3.5, and its third in the range 4.5 to 5.5.

;; Remember that that is actually only one of the six distinct orders it could
;; produce the data, so we'd actually expect that if we drew three points from
;; the distribution, we'd get data like that about one time in 40.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's continue in the 0.03, 0.007 direction:
(make-new-window "twin gaussians")
(try-ms (+ 1 (* 0.03 20)) (+ 4 (* 0.007 20))) ; -5.481165444654136
(try-ms (+ 1 (* 0.03 30)) (+ 4 (* 0.007 30))) ; -5.506903305680041

;; Darn, that's actually got worse, so we need to reevaluate our direction

;; Our best point so far is 1.6, 4.14
(make-new-window "twin gaussians")
(try-ms 1.6 4.14) ; -5.481165444654136

(lldg-change 1.6 4.14 0.01 0) ; 1.321861857217499E-5
(lldg-change 1.6 4.14 0 0.01) ; 0.0016007117192540221

;; looks like our favourite direction is now 0,0.001,
;; but we can only expect small improvements from now on.

(try-ms 1.6 4.141) ; -5.481000063373828
(try-ms 1.6 4.142) ; -5.480835863989811
(try-ms 1.6 4.143) ; -5.480672845802104
;;....
(try-ms 1.6 4.148) ; -5.479875448224991
(try-ms 1.6 4.160) ; -5.478081575575929
(try-ms 1.6 4.170) ; -5.476715256546079
(try-ms 1.6 4.180) ; -5.475465048090722
(try-ms 1.6 4.190) ; -5.474330210893202
(try-ms 1.6 4.220) ; -5.471610363888061

;; We are getting tiny improvements with every step in the 0,1 direction
;; (i.e. every time we move the right hump a bit more to the right?)

;; But I'm getting bored with this slow-and-steady progress

;; How might we guess how many steps to make before the improvement stops?

;; Suppose we're back in the situation where our best guess so far is 1, 4
(make-new-window "twin gaussians")
(try-ms 1 4) ; -5.623243186987146

;; Then earlier, we worked out what direction to go in from there like so:

(lldg-change 1 4 0.1 0) ; 0.033849244645473675
(lldg-change 1 4 0 0.1) ; 0.007052710759177572

;; And then we can say that the direction (33,7) is the best one to go in.

;; If we move a distance 1 in the direction 33,7, what changes to the coordinates would we make?

;; By dividing a vector by its length, we can make a vector of length one in the same direction
(defn length-squared [dx,dy] = (+ (* dx dx) (* dy dy)))
(defn length [dx,dy] (Math/sqrt (length-squared dx dy)))

(/ 33 (length 33 7)) ; 0.9782341251024412
(/ 7  (length 33 7)) ; 0.20750420835506328

;; Let's check that!
(length 0.978 0.207) ; 0.9996664443703209 ; close enough for government work

;; So we can make a function to turn vectors into unit vectors in the same direction
(defn unit-vector [[dx dy]]
  (let [l (length dx dy)]
    [(/ dx l) (/ dy l)]))

(unit-vector [1 5]) ; [0.19611613513818404 0.9805806756909202]
(length 0.196 0.980) ; about 1


;; And now we can wrap that up to make a direction finding function

(defn unit-direction-to-go-in [m1 m2]
  (let [dx (lldg-change m1 m2 0.0001 0)
        dy (lldg-change m1 m2 0 0.0001)]
    (unit-vector [dx dy])))

;; We start from 1,4, and if we want to move a distance of 1 in m1 m2 space, then 

(unit-direction-to-go-in 1 4) ; [0.9267081516750936 0.3757818537648294]

;; is the amount we need to add to m1 and m2

;; But how much change are we expecting when we go in this direction?

(defn change-for-small-step [m1 m2 step]
  (let [[dx dy] (unit-direction-to-go-in m1 m2)]
    (-
     (lldg (+ m1 (* step dx)) (+ m2 (* step dy)))
     (lldg m1 m2))))

(change-for-small-step 1 4 0.00001) ; 3.961153764997505E-6
(change-for-small-step 1 4 0.0001) ; 3.9609271810014945E-5
(change-for-small-step 1 4 0.001) ; 3.958661797289764E-4
(change-for-small-step 1 4 0.01) ; 0.003936053241869075
(change-for-small-step 1 4 0.1) ; 0.03713992121104681

;; Notice how as the jump size gets ten times bigger, the effect size also
;; gets (about) ten times bigger.

;; It seems that we can predict the effect of a largish jump (0.1)
;; from the effect of a small jump.

;; But of course this can't go on for ever!

(change-for-small-step 1 4 0.1) ; 0.03713992121104681
(change-for-small-step 1 4 1) ; 0.12767056083647343
(change-for-small-step 1 4 10) ; -37.168227127341524

;; What we'd like to do is to work out the best step size for our actual jump.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The insight we need is that if we make two small steps in a direction, then
;; the size of the change will be different.

;; Say we move 0.001, and get an improvement of 1, and then we move another
;; 0.001, and get an improvement of only 0.99, then we might predict that we can
;; take another 99 steps in that direction before the improvement stops.

;; Let's try that calculation for the starting point of 1,4

;; Here is the value at 1,4

(lldg 1 4) ; -5.623243186987147

;; And here is the direction to go in

(def direction (unit-direction-to-go-in 1 4)) ; [0.926708151670451 0.3757818537762786]

;; Let's try tiny shifts of only 0.001
(def step-size 0.001)

(def tiny-shift (map #(* step-size %) direction)) ; (9.267081516750935E-4 3.7578185376482945E-4)

(def original [1 4])
(def first-move (map + tiny-shift original)) ;(1.000926708151675 4.000375781853765)
(def second-move (map + tiny-shift first-move)) ;(1.0018534163033501 4.00075156370753)

(apply lldg original) ; -5.623243186987146
(apply lldg first-move) ; -5.622847320807418
(apply lldg second-move) ; -5.622451957851547

(def first-change (- (apply lldg first-move) (apply lldg original)))
(def second-change (- (apply lldg second-move) (apply lldg first-move)))

first-change ; 3.9586617972808824E-4
second-change ; 3.9536295587083714E-4

(def change-in-change (- first-change second-change))

change-in-change ; 5.032238572511005E-7

;; So, the first change upped the log-likelihood by about 0.0004
;; But the second change was less effective by 0.0000005, about

;; How many small steps should we take?

(/ first-change change-in-change) ; 786.6601990822495

;; I reckon about 786!

(def large-shift (map #(* 786 step-size %) direction))

large-shift ; (0.7283926072166236 0.29536453705915594)

(try-ms 1.072 4.029) ; -5.593989431103344

;; Not a huge improvement, but once we've moved, we can do that again

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


(make-new-window "twin gaussians")
(try-ms 0 1) ; -14.272242677972605
(long-jump [0 1]) ; [0.30129196761367955 3.649313601341553]
(try-ms 0.30 3.64) ; -6.140837771351674
(long-jump [0.30 3.64]) ; [1.0219133430621035 4.2500383831491035]
(try-ms 1.02 4.25) ; -5.625207150675367
(long-jump [1.02 4.25]) ; [1.4444763935283733 4.038760105549059]
(try-ms 1.44 4.03) ; -5.5122742477642
(long-jump [1.44 4.03]) ; [1.5343572901054818 4.271204161019856]
(try-ms 1.53 4.27) ; -5.475471025774604

;; of course, we don't need to do that by hand
(clojure.pprint/pprint (take 20 (iterate long-jump [0 1])))
;; ([0 1]
;;  [0.30129196761367955 3.649313601341553]
;;  [1.030619992805806 4.250038855786681]
;;  [1.4503906954974681 4.04297307146874]
;;  [1.5432187285370134 4.275384272499438]
;;  [1.6558202161778024 4.26326249248495]
;;  [1.656783523813035 4.309272637347416]
;;  [1.6763496323798284 4.3102464949742325]
;;  [1.6762244245146412 4.317610028219312]
;;  [1.6793033497161562 4.318153134746945]
;;  [1.6794204827755568 4.31910280896556]
;;  [1.6798346486381617 4.319438214114265]
;;  [1.6798408065025323 4.319445587479987]
;;  [1.6798410672612312 4.319445897752197]
;;  [1.6798410776607982 4.319445910123287]
;;  [1.6798410780742687 4.319445910615137]
;;  [1.6798410780876065 4.3194459106310035]
;;  [1.6798410780914173 4.319445910635537]
;;  [1.679841078088559 4.319445910632137]
;;  [1.6798410780895119 4.31944591063327])


;; Our means stopped moving on about the twelfth step

;; But in fact, the log-likelihood got about as good as it's going to get on the sixth step.
(make-new-window "twin gaussians") ; nil
(doall (for [[m1 m2] (take 12 (iterate long-jump [0 1]))]
  (try-ms m1 m2))) ; (-14.272242677972605 -6.133970056348316 -5.620489321182896 -5.50814481806791 -5.474023531548225 -5.467922926516169 -5.466778441920396 -5.466589366179929 -5.466557628664705 -5.466552068493126 -5.466551219191045 -5.466550972907067)
    
;; It looks as though the best model for {1 3 5} out of all the possible double-gaussians
;; is the one where one hump is at 1.67 and the other is at 4.31

(Math/exp -5.46) ; 0.004253555744815125

;; And that's got about a 0.4% percent chance of generating the data in the order 1,3,5
;; But it could also generate 3,1,5, or 5,3,1, etc, so in fact the chance of that distribution
;; generating something like 1,3,5 is around 2.4%

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Notice how the means tend to end up at the average values of clusters of points.

;; We have almost reinvented K-means, but not quite.

;; With three points, rather than one cluster of one point and one cluster of two, the two
;; gaussians have decided to split the middle point between them.

;; This algorithm is known as soft K-means






      


    
                    


















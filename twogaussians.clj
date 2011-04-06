;; First, some plotting routines that will come in handy
(use 'simple-plotter)

(defn make-new-window[name]
  (create-window name 200 100 black white -7.0 7.0 -0.1 1.0))

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
(def g (gaussian 1 0))

;; What happens if we make a distribution from two gaussians with known sigma=1,
;; but unknown means m1 and m2?

(defn double-gaussian[m1,m2]
  (let [g1 (gaussian 1 m1)
        g2 (gaussian 1 m2)]
    (fn [x] (/(+ (g1 x) (g2 x)) 2))))

;; Let's have a look at some examples

(do
  (make-new-window "twin gaussians")
  (cls)
  (mark-points #{1 3 5})
  (doseq [ i (range 7)]
    (plot-fn (double-gaussian 0 i))))

;; What about the likelihood of a data set?


(def points #{1 3 5})

(defn log-likelihood[g]
  (Math/log (reduce * (map g points))))

(defn try-ms [m1 m2]
  (let [g (double-gaussian m1 m2)]
    (plot-fn g)
    (let [ll (log-likelihood g)]
      (log-likelihood-line ll)
      ll)))

(make-new-window "twin gaussians")
(cls)
(mark-points points)

(try-ms 0 0) ; -20.25681559961402
(try-ms 0 1) ; -14.272242677972605
(try-ms 0 2) ; -9.624624626443204
(try-ms 0 3) ; -7.1237685823922785
(try-ms 1 3) ; -6.579925434070178
(try-ms 1 4) ; -5.623243186987147
(try-ms 1 5) ; -6.142439147988116
;; ok, so now I'm stuck and don't know how to make it better
;; time to use science

;; Let's take our best shot so far:

(make-new-window "twin gaussians")
(cls)
(mark-points points)

(try-ms 1 4) ; -5.623243186987147

;; and check the effect of moving to 1.1 4
(try-ms 1.1 4) ; -5.589393942341673

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

;; Here are some steps in that direction
(try-ms 1.03 4.007) ; -5.6114200093098745
(try-ms 1.06 4.014) ; -5.600057744971212
(try-ms 1.09 4.021) ; -5.589155679777338
(try-ms 1.12 4.028) ; -5.578714645178729
(try-ms 1.15 4.035) ; -5.568737060653577
(try-ms 1.18 4.042) ; -5.55922696311143

;; we're getting steady small improvements, so let's try larger jumps

(try-ms (+ 1 (* 0.03 10)) (+ 4 (* 0.007 10))) ; -5.525999290223576
(try-ms (+ 1 (* 0.03 20)) (+ 4 (* 0.007 20))) ; -5.481165444654136

;; Just pausing to think what the log-likelihood means:
(map #(Math/round (Math/exp %)) '(1 2 3 4 5 6 7 8 9 10 11 12))
;; (3 7 20 55 148 403 1097 2981 8103 22026 59874 162755)

;; e^-5.481 ~ 1/275 0.4%, or even more roughly 6 nats is 403
;; We might think of this likelihood  as meaning that
;; a double gaussian with both sigmas = 1 and means 1 and 4
;; has about a 1/275 chance of generating its first point in the range 0.5 to 1.5, its second in the range 2.5 to 3.5, and its third in the range 4.5 to 5.5.
;; Of course that gives it six distinct ways to produce the data, so we'd expect that if we drew three points from the distribution, we'd land in those ranges about one time in 40.

;; Let's continue:
(cls)
(mark-points points)
(try-ms (+ 1 (* 0.03 20)) (+ 4 (* 0.007 20))) ; -5.481165444654136
(try-ms (+ 1 (* 0.03 30)) (+ 4 (* 0.007 30))) ; -5.506903305680042

;; Darn, that's actually got worse, so we need to reevaluate our direction

;; Our best point so far is 1.6, 4.14
(try-ms 1.6 4.14) ; -5.481165444654136

(lldg-change 1.6 4.14 0.01 0) ; 1.321861857217499E-5
(lldg-change 1.6 4.14 0 0.01) ; 0.0016007117192540221

;; looks like our favourite direction is now 0,0.001,
;; but we can only expect small improvements from now on.

(try-ms 1.6 4.141) ; -5.481000063373828
(try-ms 1.6 4.142) ; -5.480835863989811
(try-ms 1.6 4.143) ; -5.480672845802105
;;....
(try-ms 1.6 4.148) ; -5.47987544822499
(try-ms 1.6 4.160) ; -5.478081575575929
(try-ms 1.6 4.170) ; -5.476715256546079
(try-ms 1.6 4.180) ; -5.475465048090723
(try-ms 1.6 4.190) ; -5.474330210893202
(try-ms 1.6 4.220) ; -5.471610363888061

;; Getting bored with slow-and-steady progress, let's deliberately
;; overshoot and then do a bisectional search.
(cls)
(mark-points points)
(try-ms 1.6 4.250) ; -5.4699011703641744
(try-ms 1.6 4.30) ; -5.469239043222673
(try-ms 1.6 4.40) ; -5.4757278738800155
(try-ms 1.6 4.50) ; -5.491874793424751

(cls) ; nil
(mark-points points) ; nil
(try-ms 1.6 4.20) ; -5.47330999424504
(try-ms 1.6 4.25) ; -5.4699011703641744
(try-ms 1.6 4.30) ; -5.469239043222673
(try-ms 1.6 4.35) ; -5.471218203375829
(try-ms 1.6 4.40) ; -5.4757278738800155

;; We were getting tiny improvements 






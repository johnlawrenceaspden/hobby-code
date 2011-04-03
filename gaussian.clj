;; the integral over R of e^{-x^2} is sqrt{tau}
;; the inverse of which is roughly 40%
(def gaussian-constant (/ (Math/sqrt (* 2 Math/PI))))

(defn gaussian [sigma, mu]
  (let [C (/ gaussian-constant sigma)]
        (fn [x]
          (let [s (/ (- x mu) sigma)
                e  (/ (* s s) 2 )]
          (* C (Math/exp (- e)))))))

(def g (gaussian 1 0))


(use 'simple-plotter)

(defn plot-fn[g]
  (ink black)
  (plot -7 0)
  (doseq [x (range -7 7 0.01)]
    (draw-to x (g x)))
  (ink lightgray)
  (axes))

(defn mark-points [points]
  (ink red)
  (doseq [p points] 
    (line p -0.1 p 1.0)))

(create-window "gaussian" 200 100 black white -7.0 7.0 -0.1 1.0)
(cls)
(plot-fn (gaussian 1/4 -2)) 
(plot-fn (gaussian 1/2 -1)) 
(plot-fn (gaussian 1 0)) 
(plot-fn (gaussian 2 1)) 
(plot-fn (gaussian 4 2)) 
(mark-points #{1 3 5})



;; What is the likelihood of the set of points
;; {1 3 5}

(def points #{1 3 5})

(defn plot-and-return-likelihood[g]
  (plot-fn g) 
  (reduce * (map g points)))

(cls)
(mark-points points) ; nil
(plot-and-return-likelihood (gaussian 1 0)) ; 1.5943246622599484E-9
(plot-and-return-likelihood (gaussian 1 1)) ; 2.882606611779325E-6
(plot-and-return-likelihood (gaussian 1 2)) ; 2.594839778602918E-4
(plot-and-return-likelihood (gaussian 1 3)) ; 0.0011629265075043143
(plot-and-return-likelihood (gaussian 1 4)) ; 2.594839778602918E-4

;; tradition demands that we do it in logs
(defn plot-and-return-ll[g]
  (plot-fn g) 
  (Math/log (reduce * (map g points))))

(cls)
(mark-points points)
(for [i '(1 2 3 4 5)]
  (plot-and-return-ll (gaussian 1 i))) ; (-12.756815599614018 -8.256815599614018 -6.756815599614018 -8.256815599614018 -12.756815599614018)

;; Our max likelihood given sigma 1
;; is clearly at mu=3
;; We can see that the central point gives us one unit of unlikeliness, and we get about 3 from each of the other two, for a total unlikeliness of about 7 (natural units)
(map #(Math/log ((gaussian 1 3) %)) '(1 3 5)) ; (-2.9189385332046727 -0.9189385332046727 -2.9189385332046727)

;; In likelihood terms, two 5% chances
;; and a 40% one.
(map (gaussian 1 3) '(1 3 5)) ; (0.05399096651318806 0.3989422804014327 0.05399096651318806)


;; We can vary sigma too, whilst holding mu
;; at its obvious favourite value of 3

(cls)
(mark-points points)
(for [sigma '(1 2 3 4 5)]
  (plot-and-return-ll (gaussian sigma 3)))

;; That makes us investigate sigma=2

(cls)
(mark-points points)
(for [sigma '(1.0 1.5 2 2.5)]
  (plot-and-return-ll (gaussian sigma 3)))

;; Homing in

(cls)
(mark-points points)
(for [sigma '(1.0 1.25 1.5 1.75 2 )]
  (plot-and-return-ll (gaussian sigma 3))) ; (-6.756815599614018 -5.986246253556648 -5.750988701716289 -5.741785412399878 -5.836257141293854)


(cls)
(mark-points points)
(for [sigma (range 1 2 0.2)]
  (plot-and-return-ll (gaussian sigma 3))) ; (-6.756815599614018 -6.08155804777366 -5.807048636008269 -5.729326487351225 -5.754743495554943 -5.836257141293854)

(defn i-range [a, b, n]
  (let [i (/ (- b a) n)]
    (map #(+ a (* i %)) (range (inc n)))))

(i-range 1 2 10) ; (1 11/10 6/5 13/10 7/5 3/2 8/5 17/10 9/5 19/10 2)

(cls)
(mark-points points)
(for [sigma (i-range 1.4 1.8 10)]
  (plot-and-return-ll (gaussian sigma 3))) ; (-5.807048636008269 -5.779757286056758 -5.759092337741213 -5.744248543246745 -5.734528553207691 -5.729326487351225 -5.7281143191735 -5.730430539950223 -5.735870678688264 -5.744079340813786 -5.754743495554943)









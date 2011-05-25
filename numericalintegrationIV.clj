;; Numerical Integration: Better Rules?

;; So far we've found a couple of approximations to 'the area under a graph'.

(defn trapezium-rule [f a b]
  (* 1/2 (- b a) (+ (f a) (f b))))

(defn simpson-rule [f a b]
  (let [midpoint (+ a (/ (- b a) 2))]
    (* 1/6 (- b a) (+ (f a) (* 4 (f midpoint)) (f b)))))

;; And a way of repeatedly splitting intervals and applying the rules to the sub-intervals
;; to produce better and better approximations.
(defn iterated-rule [rule f a b N]
  (if (= N 0)
    (rule f a b)
    (let [midpoint (+ a (/ (- b a) 2))]
      (+ (iterated-rule rule f a midpoint (dec N))
         (iterated-rule rule f midpoint b (dec N))))))


;; For these two functions, which are nice and smooth, with derivatives that
;; don't get too large, these rules produce good approximations
(defn square[x] (* x x))
(defn sine [x] (Math/sin x))

;; For this one, which isn't smooth, the approximations converge slowly:
(defn step [x]  (if (< x 1/2) 0.0 1.0))

;; For this, which is smooth, but which blows up near 0, the approximations are bad
(defn inverse [x] (/ x))


;; One approach which is often taken is to use 'more accurate' rules.

;; The trapezium rule and Simpson's rule are members of a family called 'Newton-Cotes' formulas.
;; The more complicated a Newton-Cotes formula is, the more polynomials it can integrate exactly.

;; But in fact, for nice smooth functions, they tend to be more accurate the
;; more polynomials they can integrate exactly.

;; Here are two more examples, and their estimates for the integral of sine over
;; a half circle (which should be exactly 2):

(defn simpson38-rule [f a b]
  (let [midpoint1 (/ (+ a a b) 3)
        midpoint2 (/ (+ a b b) 3)]
    (* 1/8 (- b a) (+ (f a) (* 3 (f midpoint1)) (* 3 (f midpoint2)) (f b)))))

(simpson38-rule sine 0 Math/PI) ; 2.040524284763495

(defn booles-rule [f a b]
  (let [midpoint1 (/ (+ a a a b) 4)
        midpoint2 (/ (+ a a b b) 4)
        midpoint3 (/ (+ a b b b) 4)]
    (* 1/90 (- b a) (+ (* 7 (f a)) (* 32 (f midpoint1)) (* 12 (f midpoint2)) (* 32 (f midpoint3)) (* 7 (f b))))))

(booles-rule sine 0 Math/PI) ; 1.9985707318238355

;; We can use the same approach to getting better estimates by subdividing: For
;; sine, as well as getting better estimates to start with, these rules have
;; high rates of convergence. It doesn't take many subdivisions before we start
;; to run into the limits of floating point accuracy.

(iterated-rule booles-rule sine 0 Math/PI 0) ; 1.9985707318238355
(iterated-rule booles-rule sine 0 Math/PI 1) ; 1.9999831309459855
(iterated-rule booles-rule sine 0 Math/PI 2) ; 1.9999997524545716
(iterated-rule booles-rule sine 0 Math/PI 3) ; 1.9999999961908446
(iterated-rule booles-rule sine 0 Math/PI 4) ; 1.9999999999407074
(iterated-rule booles-rule sine 0 Math/PI 5) ; 1.999999999999074
(iterated-rule booles-rule sine 0 Math/PI 6) ; 1.9999999999999853
(iterated-rule booles-rule sine 0 Math/PI 7) ; 1.9999999999999993
(iterated-rule booles-rule sine 0 Math/PI 8) ; 1.9999999999999998
(iterated-rule booles-rule sine 0 Math/PI 9) ; 1.9999999999999998


(iterated-rule simpson38-rule sine 0 Math/PI 0) ; 2.040524284763495
(iterated-rule simpson38-rule sine 0 Math/PI 1) ; 2.002009846628558
(iterated-rule simpson38-rule sine 0 Math/PI 2) ; 2.000119386415226
(iterated-rule simpson38-rule sine 0 Math/PI 3) ; 2.000007370036249
(iterated-rule simpson38-rule sine 0 Math/PI 4) ; 2.000000459216732
(iterated-rule simpson38-rule sine 0 Math/PI 5) ; 2.0000000286790867
(iterated-rule simpson38-rule sine 0 Math/PI 6) ; 2.0000000017921
(iterated-rule simpson38-rule sine 0 Math/PI 7) ; 2.000000000112001
(iterated-rule simpson38-rule sine 0 Math/PI 8) ; 2.0000000000069997
(iterated-rule simpson38-rule sine 0 Math/PI 9) ; 2.000000000000438


;; For the step function, however, boole's rule isn't really that much better
;; than the trapezium rule

(iterated-rule booles-rule step 0 1 0) ; 0.5666666666666667
(iterated-rule booles-rule step 0 1 1) ; 0.5388888888888889
(iterated-rule booles-rule step 0 1 2) ; 0.5194444444444445
(iterated-rule booles-rule step 0 1 3) ; 0.5097222222222222
(iterated-rule booles-rule step 0 1 4) ; 0.5048611111111111
(iterated-rule booles-rule step 0 1 5) ; 0.5024305555555555
(iterated-rule booles-rule step 0 1 6) ; 0.5012152777777777
(iterated-rule booles-rule step 0 1 7) ; 0.5006076388888889
(iterated-rule booles-rule step 0 1 8) ; 0.5003038194444445
(iterated-rule booles-rule step 0 1 9) ; 0.5001519097222222

;; Both seem to need double the number of points to halve their error.

(iterated-rule trapezium-rule step 0 1 0) ; 0.5
(iterated-rule trapezium-rule step 0 1 1) ; 0.75
(iterated-rule trapezium-rule step 0 1 2) ; 0.625
(iterated-rule trapezium-rule step 0 1 3) ; 0.5625
(iterated-rule trapezium-rule step 0 1 4) ; 0.53125
(iterated-rule trapezium-rule step 0 1 5) ; 0.515625
(iterated-rule trapezium-rule step 0 1 6) ; 0.5078125
(iterated-rule trapezium-rule step 0 1 7) ; 0.50390625
(iterated-rule trapezium-rule step 0 1 8) ; 0.501953125
(iterated-rule trapezium-rule step 0 1 9) ; 0.5009765625

;; Performance is similarly bad for boole's rule on the inverse function

(iterated-rule booles-rule inverse 0.0001 1 0) ; 779.9400477089192
(iterated-rule booles-rule inverse 0.0001 1 1) ; 391.7824297523124
(iterated-rule booles-rule inverse 0.0001 1 2) ; 198.04899407170583
(iterated-rule booles-rule inverse 0.0001 1 3) ; 101.52648013049377
(iterated-rule booles-rule inverse 0.0001 1 4) ; 53.607079025964396
(iterated-rule booles-rule inverse 0.0001 1 5) ; 29.984599583713347
(iterated-rule booles-rule inverse 0.0001 1 6) ; 18.501553032592827
(iterated-rule booles-rule inverse 0.0001 1 7) ; 13.071085113384171
(iterated-rule booles-rule inverse 0.0001 1 8) ; 10.635953348374398
(iterated-rule booles-rule inverse 0.0001 1 9) ; 9.647557264415854


(iterated-rule trapezium-rule inverse 0.0001 1 0) ; 4999.99995
(iterated-rule trapezium-rule inverse 0.0001 1 1) ; 2500.9997750199977
(iterated-rule trapezium-rule inverse 0.0001 1 2) ; 1251.8327765203333
(iterated-rule trapezium-rule inverse 0.0001 1 3) ; 627.5916420975113
(iterated-rule trapezium-rule inverse 0.0001 1 4) ; 315.8156999790102
(iterated-rule trapezium-rule inverse 0.0001 1 5) ; 160.27209317742054
(iterated-rule trapezium-rule inverse 0.0001 1 6) ; 82.84288624590312
(iterated-rule trapezium-rule inverse 0.0001 1 7) ; 44.46707207824598
(iterated-rule trapezium-rule inverse 0.0001 1 8) ; 25.61044440290146
(iterated-rule trapezium-rule inverse 0.0001 1 9) ; 16.499072595032356


;; So although it seems these higher order Newton-Cotes formulae are much more
;; accurate and faster converging on well behaved functions, they don't seem to
;; help much in integrating anything tricky.
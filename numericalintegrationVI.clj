;; Numerical Integration: Better Refinements?


;; Here are some very simple functions which we might want to test integration
;; methods on:
(defn square  [x] (* x x))
(defn sine    [x] (Math/sin x))
(defn step    [x] (if (< x 1/2) 0.0 1.0))
(defn inverse [x] (/ x))

;; Here are some Newton-Cotes formulae for approximate integration:

(defn trapezium-rule [f a b]
  (* 1/2 (- b a) (+ (f a) (f b))))

(defn simpson-rule [f a b]
  (let [midpoint (+ a (/ (- b a) 2))]
    (* 1/6 (- b a) (+ (f a) (* 4 (f midpoint)) (f b)))))

(defn simpson38-rule [f a b]
  (let [midpoint1 (/ (+ a a b) 3)
        midpoint2 (/ (+ a b b) 3)]
    (* 1/8 (- b a) (+ (f a) (* 3 (f midpoint1)) (* 3 (f midpoint2)) (f b)))))

(defn booles-rule [f a b]
  (let [midpoint1 (/ (+ a a a b) 4)
        midpoint2 (/ (+ a a b b) 4)
        midpoint3 (/ (+ a b b b) 4)]
    (* 1/90 (- b a) (+ (* 7 (f a)) (* 32 (f midpoint1)) (* 12 (f midpoint2)) (* 32 (f midpoint3)) (* 7 (f b))))))

;; We can use any of these rules to get estimate of the integral of a function over an interval:
(simpson-rule inverse 1 3) ; 10/9

;; If we halve the interval and use the rule over both halves, then we can use the rule to
;; get a better estimate by adding the estimates for the half-intervals
(+
 (simpson-rule inverse 1 2)
 (simpson-rule inverse 2 3)) ; 11/10

;; We can guess at the error involved in the estimate by taking the difference
;; between these two estimates, on the basis that splitting the interval usually
;; makes most of the error go away.

(- (simpson-rule inverse 1 3)
   (+
    (simpson-rule inverse 1 2)
    (simpson-rule inverse 2 3))) ; 1/90

;; So we'd expect that the first estimate is out by a bit more than 1/90, and
;; that the second is out by rather less than 1/90

;; For the inverse function, which can be integrated symbolically, we know the
;; true answer:
(- (Math/log 3) (Math/log 1)) ; 1.0986122886681098
(/ 10.0 9) ; 1.1111111111111112
(/ 11.0 10) ; 1.1

;; So the errors are really:
(- 1.0986122 10/9)  ; -0.0124989111111109  ; which is ~ 1/90
(- 1.0986122 11/10) ; -0.00138780000000005 ; which is ~ 1/900

;; This method of guessing the error is deeply suspect, and can go wrong, but I
;; won't go into details.

;; I think it's good enough for our purposes as long as the functions we want to
;; integrate are reasonably well behaved and we take small enough intervals.

;; So we can easily make a function which gives us the more refined of the two
;; estimates, together with a guess as to how close it is to the truth.
(defn approx-with-error[rule f a b]
  (let [guess (rule f a b)
        midpoint (/ (+ a b) 2)
        better-guess (+ (rule f a midpoint) (rule f midpoint b))
        error-estimate (- guess better-guess)
        abs-error-estimate (if (> error-estimate 0) error-estimate (- error-estimate))]
    [better-guess abs-error-estimate]))


;; Let's try it out on a few cases, on the particularly nasty integral of 1/x over [0.01,100]

;; This is the true answer
(- (Math/log 100) (Math/log 0.01)) ; 9.210340371976184

(approx-with-error trapezium-rule inverse 0.01 100) ; [2500.999775019998 2499.000174980002]
;; We guess 2500, and we think we're out by at most 2499, which is just true
(approx-with-error simpson-rule inverse 0.01 100) ; [835.4437770204454 832.5559396728856]
(approx-with-error simpson38-rule inverse 0.01 100) ; [627.4427811912234 624.2442845054817]
(approx-with-error booles-rule inverse 0.01 100) ; [391.7824297523125 388.1576179566068]

;; When we split the interval into two halves [0.01, 50.05] [50.05,100]
(approx-with-error trapezium-rule inverse 0.01 50.05) ; [1252.2495505293746 1250.2503495705255]
(approx-with-error trapezium-rule inverse 50.05 100) ; [0.7072645364881702 0.041486462512828726]

;; Our guess tells us that the great majority of the error is in the first sub interval
;; We might want to refine that first, before bothering with the other one:
;; We'll now split [0.01, 25.025][25.025,50.05][50.05,100]
(approx-with-error trapezium-rule inverse 0.01 25.025) ; [626.6241012183343 624.6256989814656]
(approx-with-error trapezium-rule inverse 25.025 50.05) ; [0.7083333333333333 0.04166666666666663]
(approx-with-error trapezium-rule inverse 50.05 100) ; [0.7072645364881702 0.041486462512828726]

;; Again, one subinterval seems to be responsible for the majority of our errors.

;; We could keep a list of intervals, sorted by the estimated error, and always refine the one
;; with the largest guessed error.

(defn interval->errorstruct [rule f [a b]]
  (let [[guess error-guess] (approx-with-error rule f a b)]
    [error-guess, guess, [a,b]]))

(def errorstructs (map (partial interval->errorstruct trapezium-rule inverse)
                          [[0.01,25.025][25.025 50.05][50.05 100]]))

;; And now we need a function to refine the interval with the largest error

(defn refine[rule f errorstructs]
  (let [sortedstructs (reverse (sort errorstructs))
        [_ _ [a b]] (first sortedstructs)
        remains (rest sortedstructs)
        midpoint (/ (+ a b) 2)
        subinterval1 (interval->errorstruct rule f [a midpoint])
        subinterval2 (interval->errorstruct rule f [midpoint b])] 
        (cons subinterval1 (cons subinterval2 remains))))

;; Now with every call to refine, we refine the interval with the largest error estimate
(refine trapezium-rule inverse errorstructs)
;; ([311.93889676733556 313.93570379188156 [0.01 12.5175]]
;;  [0.04159457274964806 0.7079060863675477 [12.5175 25.025]]
;;  [0.04166666666666663 0.7083333333333333 [25.025 50.05]]
;;  [0.041486462512828726 0.7072645364881702 [50.05 100]])


(def successive-trapezium-refinements (iterate (partial refine trapezium-rule inverse) errorstructs))


;; Here's what it looks like after a few iterations
(nth successive-trapezium-refinements 5)
;; ([18.81475746721543 20.764864658796014 [0.01 0.7917187499999999]]
;;  [0.040533241059784286 0.7015625070966475 [0.7917187499999999 1.5734374999999998]]
;;  [0.04109463731966401 0.7049306354620377 [1.5734374999999998 3.136875]]
;;  [0.041379306898238544 0.7066276281534741 [3.136875 6.26375]]
;;  [0.04152264848816445 0.7074793872568832 [6.26375 12.5175]]
;;  [0.04159457274964806 0.7079060863675477 [12.5175 25.025]]
;;  [0.04166666666666663 0.7083333333333333 [25.025 50.05]]
;;  [0.041486462512828726 0.7072645364881702 [50.05 100]])

        
;; We can get our best guess for the whole thing and our total error estimate
;; by reducing this list

(reduce + (map first  (nth successive-trapezium-refinements 5))) ; 19.104035002910425
(reduce + (map second (nth successive-trapezium-refinements 5))) ; 25.708968772954105


;; After a hundred refinements..
(reduce + (map first  (nth successive-trapezium-refinements 100))) ; 0.010431101535137086
(reduce + (map second (nth successive-trapezium-refinements 100))) ; 9.213824736866899

;; After a thousand refinements..
(reduce + (map first  (nth successive-trapezium-refinements 1000))) ; 1.0913238861095381E-4
(reduce + (map second (nth successive-trapezium-refinements 1000))) ; 9.210376750235199

;; That's not bad, (the real answer is 9.210340371976184), but it's running very slowly.

;; We could try with a higher order rule

(def successive-boole-refinements (iterate (partial refine booles-rule inverse) errorstructs))
(reduce + (map first   (nth successive-boole-refinements 1000))) ; 4.420942778526893E-15
(reduce + (map second  (nth successive-boole-refinements 1000))) ; 9.210340371976176

;; In this case, that seems to work very well, but the run time is appalling.

;; The problem is that we have a longer and longer list of intervals at every
;; step, and every step, we have to sort this list. That's an n^2 algorithm, which won't scale well.

;; What we should do here is use a priority queue. Clojure doesn't have an immutable version, although
;; it's possible to fake one with a sorted map.

;; But rather than do that, I'm going to drop out of the functional paradigm
;; altogther, and use the heap implementation from Java in a mutable fashion,
;; looping and popping and adding.



(defn improve-loop [rule f a b count]
  (let [pq (java.util.PriorityQueue. count (comparator (fn[a b](> (first a)(first b)))))]
    (.add pq (interval->errorstruct rule f [a b]))
    (loop [pq pq count count]
      (if (zero? count) pq
          (let [[err val [a b]] (.poll pq)
                midpoint (/ (+ a b) 2)
                aa (interval->errorstruct rule f [a midpoint])
                bb (interval->errorstruct rule f [midpoint b])]
            (doto pq
              (.add aa)
              (.add bb))
            (recur pq (dec count)))))))

;; Now we can do our calculation much faster
(defn integrate [rule f a b count]
  (let [pq (improve-loop rule f a b count)]
    [(reduce + (map first pq))
     (reduce + (map second pq))]))

(integrate booles-rule inverse 0.01 100 1000) ; [4.455637248046429E-15 9.21034037197618]


;; Let's try the same integral over the very nasty range [0.0000001, 10000000] which caused serious
;; problems for our previous methods.
;; The real answer is
(- (Math/log 10000000) (Math/log 0.00000001)) ; 34.538776394910684

;; And our approximations are:
(integrate booles-rule inverse 0.00000001 10000000 10) ; [3.797743055486256E10 3.797743056542089E10]
(integrate booles-rule inverse 0.00000001 10000000 100) ; [3.3430724324184924E-5 34.53877704296225]
(integrate booles-rule inverse 0.00000001 10000000 1000) ; [4.549938203979309E-11 34.53877639491147]
(integrate booles-rule inverse 0.00000001 10000000 10000) ; [9.361001557239845E-16 34.53877639491065]

;; For the non-stiff integrals that we started playing with, booles rule is great:
;; It's exact for quadratics, and several higher powers
(integrate booles-rule square 0 2 10) ; [0 8/3]
(integrate booles-rule (fn[x] (* x x x x)) 0 2 10) ; [0 32/5]
(integrate booles-rule (fn[x] (* x x x x x)) 0 2 10) ; [0 32/3]

;; and very good for higher powers, even with very few refinements
(integrate booles-rule (fn[x] (* x x x x x x)) 0 2 10) ; [969/8589934592 471219269093/25769803776]
(integrate booles-rule (fn[x] (* x x x x x x)) 0 2 20) ; [2127/1099511627776 60316066438099/3298534883328]

;; convergence is great for sine
(integrate booles-rule sine 0 Math/PI 10) ; [1.7383848804897184E-9 1.9999999999725113]
(integrate booles-rule sine 0 Math/PI 100) ; [3.1931922384043077E-15 1.9999999999999991]
(integrate booles-rule sine 0 Math/PI 1000) ; [2.526233413538588E-17 1.999999999999999]
(integrate booles-rule sine 0 Math/PI 10000) ; [6.32722651455846E-18 2.0]


;; I'm still quite worried about the error estimate that we made. It's only a guess, and it can be a bad guess.
;; Here are some functions that are deliberately designed to screw things up.
(defn sine80squared[x] (square (Math/sin (* x 80))))

(/ (Math/PI) 2) ; 1.5707963267948966

(integrate booles-rule sine80squared 0 Math/PI 1) ; [1.1091279850485843E-28 3.7074689566598855E-28]
(integrate booles-rule sine80squared 0 Math/PI 10) ; [0.013089969389960716 0.7853981633974437]
(integrate booles-rule sine80squared 0 Math/PI 100) ; [1.7991469747360833E-12 0.7853981633974478]
(integrate booles-rule sine80squared 0 Math/PI 1000) ; [3.207733520089899E-13 0.7853981633974484]
(integrate booles-rule sine80squared 0 Math/PI 10000) ; [3.1095566849572033E-15 0.7853981633974481]

(integrate trapezium-rule sine80squared 0 Math/PI 1) ; [3.226319244612108E-28 4.262878793991289E-28]
(integrate trapezium-rule sine80squared 0 Math/PI 10) ; [0.01573134053904405 0.19774924859401588]
(integrate trapezium-rule sine80squared 0 Math/PI 100) ; [5.4528883422580115E-5 0.19634904414812832]
(integrate trapezium-rule sine80squared 0 Math/PI 1000) ; [4.6327740574637914E-7 0.19634954102557595]
(integrate trapezium-rule sine80squared 0 Math/PI 10000) ; [5.200068066397881E-9 0.19634954084967793]

(defn sineinverse[x] (Math/sin (/ x)))
(integrate booles-rule sine20squared 0.001 10 1) ; [1.795529145642849E-6 2.9410957328822254]
(integrate booles-rule sine20squared 0.001 10 10) ; [0.2719499960148505 3.5512650253630462]
(integrate booles-rule sine20squared 0.001 10 100) ; [1.6314869508858218E-8 3.5646617645592134]
(integrate booles-rule sine20squared 0.001 10 1000) ; [1.1886778077561055E-8 5.010636358663671]
(integrate booles-rule sine20squared 0.001 10 10000) ; [2.7226909067499656E-14 5.010636358672848]

(integrate trapezium-rule sine20squared 0.001 10 1) ; [0.1586959395930424 2.9915404007850515]
(integrate trapezium-rule sine20squared 0.001 10 10) ; [0.006727699711582108 2.942286334606764]
(integrate trapezium-rule sine20squared 0.001 10 100) ; [0.08352740221903857 3.410294136542448]
(integrate trapezium-rule sine20squared 0.001 10 1000) ; [0.019942020277266624 4.967726331200218]
(integrate trapezium-rule sine20squared 0.001 10 10000) ; [3.421319498353696E-4 5.010635936689832]



(defn strange[x] (- (Math/sin (/ x)) (/ (Math/cos (/ x)) x)))

;; As it happens, this is the derivative of x sin(1/x), so the real answer over [0.001, 10]
;; should be
(- (* 10 (Math/sin 1/10)) (* 0.001 (Math/sin 1000))) ; 0.9975072869277496

(integrate booles-rule strange 0.001 10 1) ; [109.91706304856582 -108.12753277035351]
(integrate booles-rule strange 0.001 10 10) ; [0.07641821305025362 -1.0276123964492345]
(integrate booles-rule strange 0.001 10 100) ; [0.0798435700032961 1.0469088424961843]
(integrate booles-rule strange 0.001 10 1000) ; [2.0359056110968434E-6 0.9975072871949854]
(integrate booles-rule strange 0.001 10 10000) ; [1.9224976990340685E-12 0.997507286927752]














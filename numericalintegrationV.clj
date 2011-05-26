;; Numerical Integration: Better Refinements?

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


;; And here is a way to apply them to (power 2 N) subintervals
(defn iterated-rule [rule f a b N]
  (if (= N 0)
    (rule f a b)
    (let [midpoint (+ a (/ (- b a) 2))]
      (+ (iterated-rule rule f a midpoint (dec N))
         (iterated-rule rule f midpoint b (dec N))))))

;; Here are some very simple functions which we might want to test integration
;; methods on:
(defn square  [x] (* x x))
(defn sine    [x] (Math/sin x))
(defn step    [x] (if (< x 1/2) 0.0 1.0))
(defn inverse [x] (/ x))

;; We might notice that with our Newton-Cotes formulae, the change in the
;; estimate when we make a refinement is often around the same size as the
;; actual error.

;; Let's try using that to produce a method where we can say what error we'd
;; like, and if the change from refining the guess is too big, we should refine
;; more. And if we do refine more, we'll ask for half the desired error on each
;; of the two subintervals, which should mean that the total error is at least
;; comparable to the error we asked for!

(defn adaptive-rule-recurse [rule f a b desired-error]
  (let [guess (rule f a b)
        midpoint (/ (+ a b) 2)
        better-guess (+ (rule f a midpoint) (rule f midpoint b))
        error-estimate (- guess better-guess)
        abs-error-estimate (if (> error-estimate 0) error-estimate (- error-estimate))]
    (if (< abs-error-estimate desired-error) better-guess
        (let [half-desired-error (/ desired-error 2)]
          (+ (adaptive-rule-recurse rule f a midpoint half-desired-error)
             (adaptive-rule-recurse rule f midpoint b half-desired-error))))))

(adaptive-rule-recurse trapezium-rule square 0. 2 0.1) ; 2.6875
(adaptive-rule-recurse trapezium-rule square 0. 2 0.01) ; 2.66796875
(adaptive-rule-recurse trapezium-rule square 0. 2 0.001) ; 2.6669921875

(adaptive-rule-recurse trapezium-rule step 0.0001 1 0.1) ; 0.5
(adaptive-rule-recurse trapezium-rule step 0.0001 1 0.01) ; 0.5
(adaptive-rule-recurse trapezium-rule step 0.0001 1 0.001) ; 0.5
(adaptive-rule-recurse trapezium-rule step 0.0001 1 0.0001) ; 0.5

;; Here we're using the trapezium rule on the integral that we were previously unable to get
;; a good answer for: remember that the correct answer is 9.210340371976182

(adaptive-rule-recurse trapezium-rule inverse 0.0001 1 0.1)    ; 9.234002964342716
(adaptive-rule-recurse trapezium-rule inverse 0.0001 1 0.01)   ; 9.211881820961814
(adaptive-rule-recurse trapezium-rule inverse 0.0001 1 0.001)  ; 9.210518109406467
(adaptive-rule-recurse trapezium-rule inverse 0.0001 1 0.0001) ; 9.210358164670637

;; At this point, we're getting much better answers than we ever got before, but they've started
;; taking noticeable time to compute.

;; However, we still retain the option of using the higher order formulae:

(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.1) ; 9.210347324857782
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.01) ; 9.210345994304586
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.001) ; 9.210344014376869
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.0001) ; 9.21034116413936
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.00001) ; 9.210340404907965
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.000001) ; 9.210340376142819
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.0000001) ; 9.210340372376441
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.00000001) ; 9.210340372016345
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.000000001) ; 9.21034037198001
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.0000000001) ; 9.210340371976589
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.00000000001) ; 9.210340371976214
(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.000000000001) ; 9.210340371976185

;; These answers come back instantaneously, even when we're calculating the
;; previously impossible answer to the question to 12 decimal places.

;; But beware! If we ask for too much accuracy, then the effect of floating
;; point noise means that our recursion may never terminate:

(adaptive-rule-recurse booles-rule inverse 0.0001 1 0.0000000000001) ; freezes REPL!


;; At this point, I'm very tempted to say:

(defn integrate [f a b]
  (adaptive-rule-recurse booles-rule f a b 0.00000001))

;; Famously, the integral of 1/x diverges (very slowly) as x gets large
;; which makes it quite difficult to calculate for large intervals without using special tricks.

;; The integral from 1 to 1000000 of 1/x is log 1000000 - log 1

(- (Math/log 1000000) (Math/log 1)) ; 13.815510557964274

;; Let's try calculating that with our new integrate function:

(time
 (integrate (fn[x] (/ 1.0 x)) 1 1000000))

;; "Elapsed time: 293.219062 msecs"
;; 13.81551055800455

;; I think that's pretty good.

;; Unfortunately, we can still lock up the computer by asking the wrong question

(- (Math/log 100000000) (Math/log 1)) ; 18.420680743952367
;; but:
(time
 (integrate (fn[x] (/ 1.0 x)) 1 100000000)) ; Freezes REPL


;; Can you work out what's going on and what we could do about it?
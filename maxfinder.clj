;; Finding the maximum

;; Behold a function of three variables:
(defn f[[a b c]]
  (- 10 (+ (* a a) (* b b) (* c c))))

(f [1 2 3]) ; -4

;; We're interested in what happens to it when we make small changes to the values of
;; a b and c

(defn change[f]
  (fn [[a b c]]
    (fn [[da db dc]]
      (- (f [(+ a da) (+ b db) (+ c dc)])
         (f [a b c])))))

(def df123 ((change f) [1 2 3]))

(df123 [1 0 0]) ; -3
(df123 [0 1 0]) ; -5
(df123 [0 0 1]) ; -7

;; So if we have a guess at a maximum value of f

(f [1 2 3]) ; -4

;; We can see how it might be improved by small movements in various directions

(df [0.0001 0 0]) ; -2.000100000003613E-4
(df [0 0.0001 0]) ; -4.0000999999989517E-4
(df [0 0 0.0001]) ; -6.000100000012054E-4

(df [0.000001 0 0]) ; -2.0000010003684565E-6
(df [0 0.000001 0]) ; -4.0000010006480125E-6
(df [0 0 0.000001]) ; -6.000001000927568E-6

;; Notice how smaller changes produce smaller changes, and also how the first few figures seem to
;; be constant even as the exponents shrink.

;; If we divide by the value of the small change, we get an idea of how much the value will change
;; in response to any small change.

(defn direction [eps]
  (fn [f]
    (fn [[a b c]]
      (let [df ((change f) [a b c])]
        (map #(/ % eps) [(df [eps 0 0]) (df [0 eps 0]) (df [0 0 eps])])))))

;; As long as the function doesn't behave strangely, it doesn't make too much difference
;; which epsilon we use.
(((direction 0.1) f) [1 2 3]) ; (-2.1000000000000085 -4.100000000000001 -6.100000000000012)
(((direction 0.01) f) [1 2 3]) ; (-2.009999999999934 -4.009999999999891 -6.009999999999849)
(((direction 0.001) f) [1 2 3]) ; (-2.0009999999999195 -4.000999999998811 -6.000999999999479)
(((direction 0.0001) f) [1 2 3]) ; (-2.000100000003613 -4.000099999998952 -6.000100000012054)

;; So let's pick one once and for all:
(def grad (direction 0.0001))

(def gf (grad f))

(f [1 2 3]) ; -4
(gf [1 2 3]) ; (-2.000100000003613 -4.000099999998952 -6.000100000012054)

;; We can try moving in the direction of the grad to make our function value improve

(map + [1 2 3] (gf [1 2 3])) ; (-1.0001000000036129 -2.0000999999989517 -3.000100000012054)
(f [-1.0001000000036129 -2.0000999999989517 -3.000100000012054]) ; -4.00120003007536

;; That doesn't really help much! Let's make our step smaller
(def x [1 2 3])

(def x1 (map #(+ %1 (* 0.1 %2)) x (gf x)))
(f x1) ; 1.040095999706029

;; Much better!

;; What we did once, we can do twice
(def x2 (map #(+ %1 (* 0.1 %2)) x1 (gf x1)))
(f x2) ; 4.265738239031679

;; Or many times:

(defn improve [factor]
  (fn [x] (map #(+ %1 (* factor %2)) x (gf x))))

(f ((improve 0.1) x)) ; 1.040095999706029
(f ((improve 0.1) ((improve 0.1) x))) ; 4.265738239031679


;; We can keep iterating as often as we like
(take 30 (map f (iterate (improve 0.1) x)))

;; The function seems to settle down after about 20 iterations.

;; I worry a bit about the arbitrary choice of 0.1. That needed to be chosen by hand.
;; If we try a smaller one, 
(take 2 (drop 300 (map f (iterate (improve 0.001) x))))
;; then our improvement takes much longer
;; If we try a larger one
(take 5 (map f (iterate (improve 0.5) x))))
;; Then it's instantaneous

;; But if we over-reach
(take 50 (map f (iterate (improve 1.0) x)))
;; nothing happens at all.

;; Also, there's no reason why the best value should always be the same at every step
;; How might we go about guessing it?












































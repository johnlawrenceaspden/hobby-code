;; What is the variance of a binomial distribution?

;; Imagine a single coin toss.

;; P(1) = f
;; P(0) = (1-f)


;; What is the mean?

;; 1*f+0*(1-f) = f

;; What is the variance?

;; (1-f)^2 * f + (f)^2 * (1-f)
;; (1-2f+f^2)*f +f^2 -f^3
;; f -2f^2 +f^3 +f^2 - f^3
;; f -f^2
;; f(1-f)

;; Suppose the coin is 2:1 heads:tails

;; P(1) = 2/3
;; P(0) = 1/3

;; mean is 2/3, obviously

;; 2/3 (1/3)^2 + 1/3 (2/3)^2

;; 2/3*1/3*(1/3+2/3)
;; 2/9

;; 3 tosses of this coin

;; results heads probability difference from mean dfm^2
;; HHH 3 8/27 1   1
;; HHT 2 4/27 0   0
;; HTH 2 4/27 0   0
;; HTT 1 2/27 -1  1
;; THH 2 4/27 0   0
;; THT 1 2/27 -1  1
;; TTH 1 2/27 -1  1
;; TTT 0 1/27 -2  4

;; Expectation

(* 1/27 (+ (* 8 3) (* 4 2) (* 4 2) (* 2 1) (* 4 2) (* 2 1) (* 2 1) (* 1 0))) ; 2

;; Variance

(* 1/27 (+ (* 8 1) (* 2 1) (* 2 1) (* 2 1) (* 1 4))) ;; 2/3

;; variance is (* 3 1/3 2/3) ;; 2/3


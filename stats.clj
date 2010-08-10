;; For the last few months I've been enjoying the hottest English summer, playing cricket, tennis and rounders, and going for long cycles in the sun
;; About three weeks ago I broke my finger, and all these activities are now closed to me.
;; And now it's started raining.

;; In order to avoid gaining vast amounts of weight, as my calorie needs go from 4000 a day (sporty type) to 2000 (computer nerd), 
;; I've tried recording what I do every day and what I eat, and calculating calories in and calories out.

;; The reason that I'm doing this, of course, is that I hope that by doing so I'll influence my eating behaviour.
;; If I just carry on eating as normal, then I'm going to have a calorie excess of 2000/day. One gram of human fat stores 7 calories.
;; 2kg of fat per week sounds like quite a lot to gain. 

;; Something must be done.


;; Here are two sequences, which represent calorie deficit and weight loss for me over the last few weeks.
;; It occurs to me that this gives me a natural opportunity to test the calories in calories out theory of weight loss, which I believe to be true
;; in combination with my own skill at counting calories in and estimating calories out, which I believe to be typical for someone who's never tried it before.

(def calorie-deficit '( -230 -296 -254 -149 -109 6 2 67 119 171 138 77 -147 -94 -229 8 -89 -50 -479 -1085 -1152 -1054))

(def weight-loss '( -97 -145 -217 -246 -208 -138 -146 -215 -186 -170 -123 -38  -200 -323 -308 -314 -199 -177 -237 -197 -229 -224))

;;paranoid check

(map count (list calorie-deficit weight-loss))

;; I'd like to know the correlation.

(defn inner-product [sa sb]  
  (reduce + (map * sa sb)))

(map #(apply inner-product %) '(((0)(0))  ((0 1)(1 0))  ((1 0)(1 0)) ))

(defn mean [sa] (/(reduce + sa) (count sa)))

(map mean (list '(0) '(0.8) '(0 1)))

(defn variation [sa]
     (let [mean (mean sa)]
       (map #(- % mean) sa)))

(map variation (list '(0) '(1) '(0 1) '(7 8 9)))
(variation calorie-deficit)


(defn sample-covariance [sa sb] (/ (inner-product (variation sa) (variation sb)) 
                                   (- (count sa) 1)))
(defn sample-variance [sa] (sample-covariance sa sa))
(defn sample-standard-deviation [s] (Math/sqrt (sample-variance s)))
(defn sample-correlation [sa sb] (/ (sample-covariance sa sb) (sample-standard-deviation sa) (sample-standard-deviation sb)))




;; To know the correlation, I need to know the covariance







;; Some sanity checks






(map #(apply sample-covariance %) '(((0)(0))  ((0 1)(1 0))  ((1 0)(1 0)) ))


(map sample-variance '((0) (1) (0 0) (0 1) (0 1 2)))



(sample-standard-deviation '(100 110 90))

;;1  (sample-standard-deviation '(100))
1 1 (sample-standard-deviation '(95 105))
1 2 1 (sample-standard-deviation '(90 100 100 110))
1 3 3 1 (sample-standard-deviation '(85 95 95 95 105 105 105 115))
1 4 6 4 1 (sample-standard-deviation (concat (repeat 1 80) (repeat 4 90) (repeat 6 100) (repeat 4 110) (repeat 1 120)))

(defn pascal [n]
  (if (= n 0) '(1)
      (let [p1 (pascal (- n 1))]
        (map + (concat p1 '(0)) (cons 0 p1)))))

(pascal 6)

(defn pascal-vals [n mean step] (take (+ n 1) (iterate #(+ step %) (- mean (* (/ step 2) n )))))

(pascal-vals 5 100 10)

(defn pascal-distribution [n mean step] (mapcat #(repeat %1 %2) (pascal n) (pascal-vals n mean step)))

(sample-standard-deviation (pascal-distribution 3 0 1))
(sample-variance (pascal-distribution 3 0 1))

(defn cointosses[n] (pascal-distribution n (/ n 2) 1))

(map cointosses (range 1 5))

(sample-standard-deviation (cointosses 16))

(sample-correlation '(1 2 3) '(1 2 3))
(sample-correlation '(1 2 3) '(-1 -2 -3))
(sample-correlation '(1 0 1) '(-1 -2 -3))
(sample-correlation '(1.0002 1 1.0001) '(-1 -2 -3))

(sample-correlation calorie-deficit weight-loss)



















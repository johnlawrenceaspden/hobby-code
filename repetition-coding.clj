;; Channel has probability 0.3 of flipping a bit

;; Send 1 as 111

;; Comes back as

;; 111 (0.7)^3
;; 110 (0.7)^2 0.3
;; 101 
;; 011
;; 001
;; 010
;; 100
;; 000 


;; P(1| 111) is P(1)*P(111 | 1)/(P(1)*P( 111 | 1) + P(0)*P(111|0))

;; Which if bits are equally likely is ppp/ (ppp+qqq)

(defn likelihood111 [ p ]
  (let [q (- 1 p)]
    (/ (* p p p) (+ (* p p p) (* q q q)))))

(likelihood111 0.7) ; 0.927027027027027

;; So if you see 111 there's a 93% chance that the original bit was a 1 and a 7% chance of a 0

;; P(1| 011) is P(011 | 1) / (P 011 | 0) 

(defn likelihood011 [ p ]
  (let [q (- 1 p)] (/ (* q p p) (+ (* q p p) (* p q q)))))

(likelihood011 0.7) ; 0.7

(likelihood011 0.9)

(/ (* 0.9 0.9 0.1) (+ (* 0.9 0.9 0.1) (* 0.9 0.1 0.1)))
(/ (* 0.9 0.1) (+ (* 0.9 0.1) (* 0.1 0.1)))
(/ (* 0.9) (+ (* 0.9) (* 0.1)))


;; What is the error probability of this code?

;; 1 -> 111 ->

;; 111 (0.7)^3
;; 110 (0.7)^2 0.3
;; 101 
;; 011
;; 001
;; 010
;; 100
;; 000

;; -> 1

(defn flip-probability [p]  (let [q (- 1 p)] (+ (* p p p) (* 3 (* p p q)))))

(map flip-probability '(0.999 0.99 0.95 0.9 0.8 0.7 0.6 0.5 0.4))
;; (0.999997002 0.999702 0.99275 0.9720000000000001 0.8960000000000001 0.7839999999999998 0.6479999999999999 0.5 0.35200000000000004)



;; Must show that if p>0.5 then p^3+3p^2(1-p) > p
(use 'simple-plotter)

(do 
  (create-window "p^3+3p^2(1-p) vs p" 300 200 white black 0 1 0 1)
  (axes)
  (doseq [ f (list (fn [p] p)
                   (fn [p] (+ (* p p p) (* 3 p p (- 1 p)))))]
    (doseq [[x y] (let [xs (range 0 1 0.01)]
                    (partition 2 (interleave xs (map f xs ))))]
      (plot x y))))


;; Always positive because the cubic factorizes:

;; (p^3 + 3p^2(1-p) - p )
;; (p^3 - 3 p^3 + 3p^2 - p)
;; (-2p^3 + 3p^2 -p)
;; p( -2p^2 + 3p -1 )
;; p(p-1)(1-2p)



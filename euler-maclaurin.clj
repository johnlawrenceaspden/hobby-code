;; Euler-Maclaurin summation

;; Think of a sequence as a function from N to R
(defn sq1 [n] n)

(sq1 0) ;-> 0

;; To turn this into a standard clojure lazy-sequence we'll have a 'realize' function

(defn realize [sq]
  (map sq (range)))

(realize sq1) ;-> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 ...)

;; We have a left-shift operator
(defn L [sq]
  (fn[n] (sq (inc n))))

;; Operator composition can be done with comp
(def L2 (comp L L)) 

(realize (L sq1)) ;-> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 ...)
(realize (L2 sq1)) ;-> (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 ...)
(realize ((apply comp (repeat 5 L)) sq1)) ;-> (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 ...)


;; We also have an indefinite summation operator
(defn sigma [sq]
  (fn [n] (reduce + (map sq (range n)))))

((sigma sq1) 10) ;-> 45

(realize (sigma sq1)) ;-> (0 0 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210 231 253 276 300 325 ...)

;; Obviously Schlemiel the Painter is at work here. Whatever. This isn't production code.

;; We can also make an identity operator
(defn I[sq] (fn[n] (sq n)))

;; Operators can be added ((+ A B) sq) == ( + (A sq) (B sq))
(defn add [op1 op2]
  (fn [sq] (fn [n] (+ ((op1 sq) n) ((op2 sq) n)))))

(realize ((add L I) sq1)) ;-> (1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 ...)

;; And subtracted
(defn minus [op1 op2]
  (fn [sq] (fn [n] (- ((op1 sq) n) ((op2 sq) n)))))

;; The finite-difference operator 
(def delta (minus L I)) 

(realize (delta sq1)) ;-> (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ...)

;; Is in some sense the opposite of the indefinite summation operator

(realize sq1) ;-> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 ...)
(realize (delta (sigma sq1))) ;-> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 ...)
(realize (sigma (delta sq1))) ;-> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 ...)

;; We write sigma = 1 / (L - 1)

;; Left shifting is the exponential of differentiation, so 

;; sigma = 1/(exp(D)-1) = - (

;; There's a Laurent Series for 1/(exp(x)-1) = 1/x -1/2 + x/12 - x^3/720 + x^5/30240 - x^7/1209600 + .... 

;; And the inverse of differentiation is integration

;; So by analogy

;; sigma = integration - I/2 + D/12 + DDD/720 + DDDDD/30240 + ...

;; Utter bullshit.

;; And yet:

(defn squares[n] (* n n)) ;-> #'user/squares

(defn integralsquares [n] (/ (* n n n) 3)) ;-> #'user/integralsquares
(defn derivativesquares [n] (* 2 n)) ;-> #'user/derivativesquares
(defn doublederivativesquares [n] 2) ;-> #'user/doublederivativesquares
(defn triplederivativesquares [n] 0) ;-> #'user/triplederivativesquares

(realize squares) ;-> (0 1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 ...)

(realize integralsquares) ;-> (0 1/3 8/3 9 64/3 125/3 72 343/3 512/3 243 1000/3 1331/3 576 2197/3 2744/3 1125 4096/3 4913/3 1944 6859/3 8000/3 3087 10648/3 12167/3 4608 15625/3 17576/3 ...)
(map #(- (* 1/2 %)) (realize squares)) ;-> (0N -1/2 -2N -9/2 -8N -25/2 -18N -49/2 -32N -81/2 -50N -121/2 -72N -169/2 -98N -225/2 -128N -289/2 -162N -361/2 -200N -441/2 -242N -529/2 -288N -625/2 -338N ...)
(map #(/ % 12) (realize derivativesquares)) ;-> (0 1/6 1/3 1/2 2/3 5/6 1 7/6 4/3 3/2 5/3 11/6 2 13/6 7/3 5/2 8/3 17/6 3 19/6 10/3 7/2 11/3 23/6 4 25/6 13/3 ...)
(map #(/ % -720) (realize triplederivativesquares)) ;-> (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ...)


(map + 
     (realize integralsquares)
     (map #(- (* 1/2 %)) (realize squares))
     (map #(/ % 12) (realize derivativesquares)) 
     (map #(/ % -720) (realize triplederivativesquares))) ;-> (0N 0N 1N 5N 14N 30N 55N 91N 140N 204N 285N 385N 506N 650N 819N 1015N 1240N 1496N 1785N 2109N 2470N 2870N 3311N 3795N 4324N 4900N 5525N ...)

(realize (sigma squares)) ;-> (0 0 1 5 14 30 55 91 140 204 285 385 506 650 819 1015 1240 1496 1785 2109 2470 2870 3311 3795 4324 4900 5525 ...)

;; I am not quite sure what to make of all this....

(defn euler-maclaurin[integral function derivative triplederivative]
  (map + 
       (realize integral)
       (map #(- (* 1/2 %)) (realize function))
       (map #(/ % 12) (realize derivative)) 
       (map #(/ % -720) (realize triplederivative))))

(defn cube[n] (* n n n))
(defn intcube[n] (/ (* n n n n) 4))
(defn dcube[n] (* 3 n n))
(defn ddcube[n] (* 3 2 n))
(defn dddcube[n] (* 3 2 1))

(euler-maclaurin intcube cube dcube dddcube) ;-> (1/120 1/120 121/120 1081/120 4321/120 12001/120 27001/120 52921/120 94081/120 155521/120 243001/120 363001/120 522721/120 730081/120 993721/120 1323001/120 1728001/120 2219521/120 2809081/120 3508921/120 4332001/120 5292001/120 6403321/120 7681081/120 9141121/120 10800001/120 12675001/120 ...)
(realize (sigma cube)) ;-> (0 0 1 9 36 100 225 441 784 1296 2025 3025 4356 6084 8281 11025 14400 18496 23409 29241 36100 44100 53361 64009 76176 90000 105625 ...)

;;; you're supposed to take off the f(a) terms as well as adding in the f(b) terms. tired. fix later.

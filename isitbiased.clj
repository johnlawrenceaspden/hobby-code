;; Sitting in Waterstone's trying to work out whether a 
;; 20p piece in my wallet is fair or not.

(defn factorial [n] (if (< n 2) 1 (* n (factorial (dec n)))))
(defn choose [ n r ]  (/ (factorial n) (factorial r) (factorial (- n r))))

;; Two hypotheses. Coin is fair. Coin is biased.
;; Prior is equal weight to the two hypotheses, with a uniform prior over the
;; biases p in the biased model.

;; Assuming a uniform prior over coin-biases
;; This is the ratio between the 
(defn unbiased-ratio [a b]
  (double
   (/ (* (+ a b 1) (choose (+ a b) a))
      (reduce * (repeat (+ a b) 2)))))

(unbiased-ratio 7 3) ; 1.2890625
(unbiased-ratio 16 12) ; 3.286566194146872
(unbiased-ratio 21 17) ; 4.08350239675201
(unbiased-ratio 22 21) ; 5.262583863650434
(unbiased-ratio 28 22) ; 4.020109204189914
(unbiased-ratio 32 24) ; 3.444977734855868
(unbiased-ratio 37 27) ; 2.98325836695088
(unbiased-ratio 42 31) ; 3.0315224808411
(unbiased-ratio 46 36) ; 3.986026666275192
(unbiased-ratio 51 39) ; 3.451814487494289
(unbiased-ratio 54 44) ; 4.798955366016645
(unbiased-ratio 57 49) ; 6.132629163735339
(unbiased-ratio 62 51) ; 5.0177729810111
(unbiased-ratio 67 55) ; 4.933498327397281

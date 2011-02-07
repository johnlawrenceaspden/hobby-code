;; Sitting in Waterstone's trying to work out whether a 
;; 20p piece in my wallet is fair or not.

;; Carefully spinning it on edge, letting it spin for a while and then collapse.
;; Anything dodgy such as falling off the pad or not spinning for long is a re-spin.

(defn factorial [n] (if (< n 2) 1 (* n (factorial (dec n)))))
(defn choose [ n r ]  (/ (factorial n) (factorial r) (factorial (- n r))))

;; Two hypotheses. Coin is fair. Coin is biased.
;; Prior is equal weight to the two hypotheses, with a uniform prior over the
;; biases p in the biased model.

;; This is the ratio between the probability masses of the two posteriors
(defn unbiased-ratio [a b]
  (double
   (/ (* (+ a b 1) (choose (+ a b) a))
      (reduce * (repeat (+ a b) 2)))))

(unbiased-ratio 7 3) ; 1.2890625                ;7 heads, 3 tails, unbiased:biased 1.28:1
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

;; first experiment terminated with 67 heads and 55 tails, but coin saved

(unbiased-ratio 2 5) ; 1.3125      2 heads, 5 tails, unbiased 1.31:1
(unbiased-ratio 7 9) ; 2.967529296875
(unbiased-ratio 11 14) ; 3.45386266708374
(unbiased-ratio 16 16) ; 4.618347825016826
(unbiased-ratio 21 19) ; 4.895426849907381
(unbiased-ratio 21 19) ; 4.895426849907381
(unbiased-ratio 26 20) ; 3.745795870256956
(unbiased-ratio 29 25) ; 5.138974305228603
(unbiased-ratio 34 27) ; 4.245406137583945

;; second experiment terminated after 34 heads and 27 tails

;;total is 101 heads, 82 tails
(unbiased-ratio 101 82) ; 4.056481237956995



















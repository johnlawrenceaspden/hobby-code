;; Spock and the coins

;; Spock's priors

;; Even chances for coins biased to heads (Ch) or for coins biased to tails (Ct)
;; P(Ch)=1/2
;; P(Ct)=1/2

;; Given Ch, coin comes up heads 2/3, tails 1/3

;; P(H | Ch)= 2/3
;; P(T | Ch)= 1/3

;; Given Ct coin comes up tails 2/3, heads 1/3

;; P(H | Ct) = 1/3
;; P(T | Ct) = 1/3

;; Observed sequence HHHHTHTHTH

;; P(HHHHTHTHTH | Ch) =
(reduce * (map (fn[a] (if (= a \H) 2/3 1/3)) "HHHHTHTHTH")) ; 128/59049

;; P(HHHHTHTHTH | Ct) =
(reduce * (map (fn[a] (if (= a \H) 1/3 2/3)) "HHHHTHTHTH")) ; 8/59049

;; P( Ch | HHHHTHTHTH ) = P(Ch)*P(HHHHTHTHTH | Ch) / (P(Ch)*P(HHHHTHTHTH | Ch) + P(Ch)*P(HHHHTHTHTH))
;; = 1/2*128/59049 / (1/2*128/59049 + 1/2*8/59049)
;; = 128/(128+8) = 16/17

;; or 16:1, expressed in terms of odds.
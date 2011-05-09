(defn gaussian [mu sigma]
  (fn [x]
    (let [ssd (/ (- x mu) sigma)
          sd (* ssd ssd)
          factor (/ 1 (Math/sqrt (* 2 Math/PI)) sigma)]
      (* factor (Math/exp (* -1/2 sd))))))

;; sanity check
(map (gaussian 0 1) (range -1 1 0.1))

(defn likelihood [xs mu sigma]
  (reduce * (map (gaussian mu sigma) xs)))

;; A quick maximum likelihood
(likelihood [0 0 0] 0 1) ; 0.06349363593424098
(likelihood [0 0 0] 0 0.1) ; 63.49363593424098
(likelihood [0 0 1] 0 1) ; 0.038510836890748947
(likelihood [-1 0 1] 0 1) ; 0.02335800330543158
(likelihood [-1 0 1] 0 2) ; 0.006181111673204696
(likelihood [-1 0 1] 0 0.1) ; 2.362011496691831E-42
(likelihood [-1 0 1] 0 0.4) ; 0.001915180501771747
(likelihood [-1 0 1] 0 0.5) ; 0.009303412060034514
(likelihood [-1 0 1] 0 0.6) ; 0.018276914721837227
(likelihood [-1 0 1] 0 0.7) ; 0.024050317175942977
(likelihood [-1 0 1] 0 0.8) ; 0.025994119342662193
(likelihood [-1 0 1] 0 0.9) ; 0.025341752327009883
(likelihood [-1 0 1] 0 0.81) ; 0.026022066004237898
(likelihood [-1 0 1] 0 0.82) ; 0.02602564769190768
(likelihood [-1 0 1] 0 0.83) ; 0.026006302784975496
(likelihood [-1 0 1] 0 0.811) ; 0.02602349662698512
(likelihood [-1 0 1] 0 0.812) ; 0.026024685075132118
(likelihood [-1 0 1] 0 0.813) ; 0.026025632798181965
(likelihood [-1 0 1] 0 0.814) ; 0.02602634124274292
(likelihood [-1 0 1] 0 0.815) ; 0.026026811852466962
(likelihood [-1 0 1] 0 0.816) ; 0.026027046067989387
(likelihood [-1 0 1] 0 0.817) ; 0.026027045326869728



;; Directly calculating we're playing off sigma against average of residuals squared
;;we want mu=0 and sigma = sqrt(2/3)
(Math/sqrt 2/3) ; 0.816496580927726

;; Which convinces me that my gaussian formula's right!

;; What about the a priori?

;; An improper prior flat in mu and in log sigma
(defn prior [mu sigma]
  (/ sigma))


(defn apriori [xs mu sigma]
  (* (likelihood xs mu sigma) (prior mu sigma)))

;; to find P(xs|sigma) we want to marginalize over mu

;; Approximate the integral over all mu with
(defn marginal [xs sigma]
  (reduce + (map #(apriori xs % sigma) (range -3 3 0.01))))

(marginal [-1 0 1] 0.1) ; 3.418308964574561E-40
(marginal [-1 0 1] 1) ; 3.38037544294389
(marginal [-1 0 1] 10) ; 0.003614134977996703
(marginal [-1 0 1] 2) ; 0.8862513580105127
(marginal [-1 0 1] 0.5) ; 1.3463921276626964
(marginal [-1 0 1] 0.4) ; 0.27716540275751367
(marginal [-1 0 1] 0.6) ; 2.645039684435177
(marginal [-1 0 1] 0.7) ; 3.480567936206956
(marginal [-1 0 1] 0.8) ; 3.761875473207476
(marginal [-1 0 1] 0.8) ; 3.761875473207476
(marginal [-1 0 1] 0.9) ; 3.6674647284282695
(marginal [-1 0 1] 0.81) ; 3.765919920878967
(marginal [-1 0 1] 0.79) ; 3.7540927814618517
(marginal [-1 0 1] 0.82) ; 3.766438263286651
(marginal [-1 0 1] 0.83) ; 3.7636386629756924
(marginal [-1 0 1] 0.815) ; 3.766606741079675

;; Hmm. Looks like it's 0.81 again, which it really shouldn't be!








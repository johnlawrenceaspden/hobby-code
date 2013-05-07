;; Probabilistic Graphical Models

;; A->B->C, all Y/N

(def phi1
  {{:A :Y} 0.5,
   {:A :N} 0.5})

(def phi2
  {{:B :Y :A :Y} 0.6
   {:B :Y :A :N} 0.4
   {:B :N :A :N} 0.6
   {:B :N :A :Y} 0.4})

(def phi3
  {{:B :Y :C :Y} 0.6
   {:B :Y :C :N} 0.4
   {:B :N :C :N} 0.6
   {:B :N :C :Y} 0.4})

(defn variables [factor] 
  (set (mapcat keys (keys factor))))

(map variables (list phi1 phi2 phi3)) ; (#{:A} #{:A :B} #{:C :B})

(defn values [var factor]
  (set (for [s (keys phi3)] (s :C))))

(defn varsvals[factor]
  (into{} (for [v (variables factor)]
            [v (values v factor)])))

(varsvals phi1) ; {:A #{:Y :N}}
(varsvals phi2) ; {:A #{:Y :N}, :B #{:Y :N}}
(varsvals phi3) ; {:C #{:Y :N}, :B #{:Y :N}}

(defn factor-compatibility [factor1 factor2]
  (let [vv1 (varsvals factor1)
        vv2 (varsvals factor2)
        commonvars (clojure.set/intersection (variables factor1) (variables factor2))]
    (every? identity (for [v commonvars] (= (vv1 v) (vv2 v))))))


(factor-compatibility phi1 phi2)


(defn factor-multiply-varsvals [factor1 factor2]
  (if (factor-compatibility factor1 factor2)
    (let [vv3 (into{} (concat (varsvals phi3) (varsvals phi2)))]
      vv3)
    'failzor))

(factor-multiply-varsvals phi1 phi2) ; {:C #{:Y :N}, :B #{:Y :N}, :A #{:Y :N}}


(seq (factor-multiply-varsvals phi1 phi2)) ; ([:C #{:Y :N}] [:B #{:Y :N}] [:A #{:Y :N}])

(defn all-combinations [vvlist]
  (if (empty? vvlist) '(())
      (let [[var range] (first vvlist)]
            (for [r range]
              (for [c (all-combinations (rest vvlist))]
                (cons (list var r) c ))))))


(all-combinations '()) ; ()
(all-combinations '([:A #{:Y :N}])) ; (((:A :Y)) ((:A :N)))
(all-combinations '([:A #{:Y :N}][:B #{:Y :N}]))











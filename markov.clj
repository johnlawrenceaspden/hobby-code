(defn it[x]
  (case x
    :A (rand-nth [:A :A :A :A :A :B])
    :B (rand-nth [:A :A :A :A :B :C])
    :C (rand-nth [:A :A :A :A :A :A])
    ))



  
(map it '(:A :B :C))

(def freq (frequencies (take 1000000 (iterate it :A))))

freq ; {:A 810246, :B 162685, :C 27069}

(float (/ (reduce + (vals freq)) (freq :C))) ;36.942627


(defn it2[x]
  (case x
    :A (rand-nth [:A :A :A :A :A :B])
    :B (rand-nth [:A :A :A :A :A :C])
    :C (rand-nth [:A :A :A :A :A :A])
    ))

(def freq2 (frequencies (take 1000000 (iterate it2 :A))))

freq2 ; {:A 836937, :B 139768, :C 23295}

(float (/ (reduce + (vals freq2)) (freq2 :C))) ; 42.927666

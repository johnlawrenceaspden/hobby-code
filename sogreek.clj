(use 'clojure.test)

(print "αβγ")

(is "αβγ""αβγ")

(defn parse [s] "αβγ,ΑΒΓ")

(deftest greek (is (= "αβγ, ΑΒΓ" (parse ""))))

(run-tests)
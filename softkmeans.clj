(def data '( 1 2 5 6 ))

(def means ' (0 10 11))

(def beta 3)

(defn distance [a b] (if (> a b) (- a b) (- b a)))

(def distances '((1 2 5 6)
                 (9 8 5 4)
                 (10 9 6 5)))

(for [m means]
  (for [d data]
    (distance m d))) ; ((1 2 5 6) (9 8 5 4) (10 9 6 5))

(defn exp-distance [a b] (Math/exp ( - (* beta (distance a b)))))

(for [m means]
  (for [d data]
    (exp-distance m d)))

;; ((0.049787068367863944 0.0024787521766663585 3.059023205018258E-7 1.522997974471263E-8)
;;  (1.8795288165390832E-12 3.775134544279098E-11 3.059023205018258E-7 6.14421235332821E-6)
;;  (9.357622968840175E-14 1.8795288165390832E-12 1.522997974471263E-8 3.059023205018258E-7))

(defn total-responsibility [means data-point]
  (reduce + (map #(exp-distance % data-point) means)))

(total-responsibility means (first data)) ; 0.049787068369837054


(defn responsibility [ m d ]
  (/ (exp-distance m d)
     (total-responsibility means d)))


(responsibility (first means) (first data)) ; 0.999999999960369

(clojure.contrib.pprint/cl-format nil "~{~{~A ~}~%~}"
                                  (for [m means]
                                    (for [d data]
                                      (responsibility m d)))) ; 

(clojure.contrib.pprint/cl-format nil "~S" 1.2)
(clojure.contrib.pprint/cl-format nil "~{~S ~}" '( 1.2 1.3 1.4))
(clojure.contrib.pprint/cl-format nil "~{~{~S ~}~}" '(( 1.2 1.3 1.4)(:a :b :c)))

(clojure.contrib.pprint/cl-format nil "~A" 1.2)
(clojure.contrib.pprint/cl-format nil "~{~A ~}" '( 1.2 21 1.4789789))
(clojure.contrib.pprint/cl-format nil "~{~{~A ~}~}" '(( 1.2 1.3 1.4)(:a :b :c)))


(clojure.contrib.pprint/cl-format nil "~D" 1.2)
(clojure.contrib.pprint/cl-format nil "~3,'0D" 1.2)
(clojure.contrib.pprint/cl-format nil "~D" "yo")



((0.999999999960369      0.9999999840117645    0.4878555511603684  0.0023556330807966803)
 (3.775134544129485E-11  1.5229979501212127E-8 0.4878555511603684  0.9503302116973793)
 (1.8795288164645958E-12 7.582560306680144E-10 0.02428889767926321 0.047314155221824035))


(exp-distance 3 5)

(defn raw-responsibility [mean datum]
  (exp-distance mean data))







  (let [ expds (map #(exp-distance % data-point) means)
         total (reduce + expds)]
    (map / expds (repeat total))))

(responsibilities means (first data))

(defn weighted-average [weights data]
  (/ (reduce + (map * weights data))
     (reduce + weights)))

(weighted-average '(0.1 0.1 1.0 0.1) '(1 2 3 4))

(weighted-average (responsibilities means (first data)) 
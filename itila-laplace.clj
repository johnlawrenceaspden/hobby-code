;; Photon detector pointed at star

;; Poisson process with 1/lambda prior

;; Receives 20 photons in one time period, what is probability distribution of lambda?

(defn seq-str [sq]  (clojure.pprint/cl-format false "~{~3f ~}" (take 40 sq)))

(defn poisson-seq [l]
  (reductions (fn[a n] (/(* a l) n))
              (Math/exp (- l))
              (iterate inc 1)))

(seq-str (poisson-seq 20))

(seq-str
 (map (fn [n] (* (Math/pow (/ n 19.0) 19)(Math/exp (- 19 n))))
      '( 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 )))
;; ".092 .18 .30 .45 .61 .77 .89 .97 1.0 .97 .91 .81 .69 .57 .46 .35 .27 .20 .14 .098 "


(/ (reduce * (range 1 (+ 19 1)))
   (* (Math/pow 19 19.5)(Math/exp -19)(Math/sqrt (* 2 Math/PI))))

(/ (*(Math/sqrt Math/PI) 2) Math/PI)

(/ (- (Math/log (Math/sqrt (/ Math/PI 2))) (Math/log 1)) (Math/log 2))

(/ (- (Math/log (* 2 (Math/sqrt Math/PI))) (Math/log Math/PI)) (Math/log 2))
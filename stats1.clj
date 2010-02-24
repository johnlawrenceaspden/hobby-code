;;Statistics Example Sheet 1
(set! *print-length* 100)
(load-file "./hobby-code/require-all-snippet.clj")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn B [n,p]
  (cond (= n 1) (if (> p (rand)) 1 0)
        :else   (+ (B (dec n) p) (B 1 p))))

(defn T[p] 
  (+ (B 1 p) (B 1 p)))

(defn average [seq]
  (* 1.0 (/ (apply + seq) (count seq))))

(defn approx-expectation [est]
  (average (for [i (repeat 1000 'a)] (est))))



(defn estimator1 [p] (- 2008 (/ (T p) 2)))

(approx-expectation #(estimator1 0.3)) ;should be (2008-0.3)

(defn estimator2 [p] (if (= (T p) 0) 1 0))

(estimator2 0.3)

(approx-expectation #(estimator2 0.3)) ;should be (1-0.3)^2

(defn estimator3 [p] (Math/pow -2 (T p)))

(approx-expectation #(estimator3 0.3)) ;should be (1-3*0.3)^2 = 0.01
(approx-expectation #(estimator3 0.5)) ;should be (1-3*0.5)^2 = 0.25
(approx-expectation #(estimator3 0.9)) ;should be (1-3*0.9)^2 = -1.7^2 = 2.89

(estimator3 0.3) ;; note that this is unbiased, but very silly
                 ;; we know that 0<p<1, but it produces estimates of 1,-2, or 4
                 ;; it's just right on average!

;; (d) is a non-existence proof, which is not really amenable to a computer!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Question 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gene [theta]
  (let [t (/ theta (+ theta 1))]
    (if (< (rand) t) \G \g)))

(for [i (range 100)] (gene 9)) ; theta 9 means 9/10 genes are G

(defn genotype [theta] ;; gG and Gg must both become "Gg"
  (apply str (sort (list (gene theta) (gene theta)))))

(genotype 3)

(defn sample [n theta]
  (for [i (range n)] (genotype theta)))

(sample 10 3)   ;G outnumbers g by three to one
(sample 10 1/3) ;g outnumbers G by three to one

(defn seq->countmap [sq]
  (reduce (fn [map key](assoc map key (inc (get map key 0))))  {}  sq ))

(seq->countmap (sample 10 0.5))

;(there is a library function in contrib)
;(use 'clojure.contrib.accumulators)
;(add-items empty-counter-with-total '( 1 2 3 3 4 ))

(sort (sample 20 0.5))

(seq->countmap (sample 1210 0.1))          ; theta 0.1  means 1/11 genes are G
(seq->countmap (sample 999 0.5))           ; theta 0.5  means 1/3  genes are G
(seq->countmap (sample 1000 1))            ; theta 1    means 1/2  genes are G
(seq->countmap (sample 999 2))             ; theta 2    means 2/3  genes are G
(seq->countmap (sample 1210 10))           ; theta 10   means 10/11  genes are G


(defn sample->abc [a-sample]
  (let [c (seq->countmap a-sample)]
    [ (get c "GG" 0) (get c "Gg" 0) (get c "gg" 0)]))

(sample->abc (sample 1000 1))

(defn likelihood [[a b c] theta] ;;big samples break this boo
  (/ (Math/pow theta (+ a a b))
     (Math/pow (+ 1 theta) (* 2 (+ a b c)))))

(defn likelihood [[a b c] theta] ;;big samples break this as well
  (/ (Math/pow (/ theta (+ 1 theta)) (+ a a b))
     (Math/pow (+ 1 theta) (+ b c c))))

(defn loglikelihood [[a b c] theta] ;;logs to the rescue
  (- (* (+ a a b) (Math/log theta))
     (* 2 (+ a b c) (Math/log (+ 1 theta)))))

(defn models-in-likelihood-order [modelslist sample]
  (map second (sort (map 
                    (fn [theta] [(loglikelihood (sample->abc sample) theta) theta])
                    modelslist))))

(models-in-likelihood-order (map #(/ % 10.) (range 100)) (sample 30 8))











  
  









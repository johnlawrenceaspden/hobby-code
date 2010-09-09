;; The probabilities in an evilfiend problem:

;; probability of a prisoner finding his passport on the first go is 1/100
;; on the second go is 99/100*1/99 = 1/100
;; on the third is (1-1/100-1/100)*1/98=1/100

;; So, if there are 3 prisoners and 2 chances each
;; 1/3 time prisoner finds first passport and then you're in 2,2 ;you've won
;; 1/3 time prisoner finds passport on second go and then your in 1,3 ;you've won
;; 1/3 time prisoner finds passport on third go and you've lost
;; otherwise you've lost.

p(3,2)= 1/3*p(2,2) + 1/3*p(1,3)+1/3*0
p(2,2)=1
p(1,3)=1

p(3,2) = 2/3

(use 'clojure.test)

(with-test
  (defn rng [start end]
    (if (> start end) (reverse (rng end start))
        (range start (inc end))))
  (is (= (rng 0 0) '(0)))
  (is (= (rng 1 1) '(1)))
  (is (= (rng 3 1) '(3 2 1)))
  (is (= (rng 1 3) '(1 2 3))))

(run-tests)

(defn p [prisoners, chances]
  (cond (<= prisoners chances) 1
        :else (* (/ prisoners)
                 (reduce +
                         (map #((resolve 'p) % chances)
                              (rng (- prisoners 1) (- prisoners chances)))))))

(def p (memoize p))

(use 'clojure.contrib.trace)

(trace '(p) (p 100 50))
(p 4 2)
        
;; (+  (reduce + (map / (range 51 101))) (p 100 50))

  
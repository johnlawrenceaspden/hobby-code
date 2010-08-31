(defn divides? [i n] (= 0 (mod n i)))

(defn factors [n]
  (if (= n 1) '()
      (loop [i 2]
        (if (= 0 (mod n i)) (cons i (factors (/ n i)))
            (recur (inc i))))))


;; (factors 1000) (2 2 2 5 5 5)
(time (factors 123456789012345678901234567890)) ;; 3500 msec

(defn factors [n]
  (loop [acc '() i 2 n n]
    (cond (= n 1)         acc
          (= 0 (mod n i)) (recur (cons i acc) i (/ n i))
          :else           (recur acc (inc i) n))))
        
;; (factors 1000) (5 5 5 2 2 2)
(time (factors 123456789012345678901234567890)) ;; 3900 msec


(defn factors [n]
  (loop [acc [] i 2 n n]
    (cond (= n 1)         acc
          (= 0 (mod n i)) (recur (conj acc i) i (/ n i))
          :else           (recur acc (inc i) n))))

;;(factors 1000) [2 2 2 5 5 5]
(time (factors 123456789012345678901234567890)) ;; 3900 msec

(defn factors [n]
  (loop [acc '() i 2 n n]
    (cond (= n 1)         acc
          (< n (* i i))   (cons n acc)
          (= 0 (mod n i)) (recur (cons i acc) i (/ n i))
          :else           (recur acc (inc i) n))))

;; (factors 1000) (2 2 2 5 5 5)
(time (factors 123456789012345678901234567890)) ;;7 msec


(defn factors [n]
  (loop [acc '() i 2 n n sqrt (Math/floor (Math/sqrt n))]
    (cond (= n 1)         acc
          (< sqrt i)      (cons n acc)
          (= 0 (mod n i)) (recur (cons i acc) i (/ n i) (Math/floor (Math/sqrt (/ n i))))
          :else           (recur acc (inc i) n sqrt))))

;; (factors 1000) (2 2 2 5 5 5)
(time (factors 123456789012345678901234567890)) ;;7 msec


(defn factors [n]
  (loop [n n ,test 2, acc '(), sofar 1]
    (if (= n 1) 
      (reverse acc)
      (if (< n (* test test))
        (reverse (cons n acc))
        (if-not (integer? (/ n test))
          (recur n (inc test) acc sofar)
          (recur (/ n test) test (cons test acc) (* sofar test)))))))

(factors 1000)

(= (map factors (range 1 100)) '(() (2) (3) (2 2) (5) (2 3) (7) (2 2 2) (3 3) (2 5) (11) (2 2 3) (13) (2 7) (3 5) (2 2 2 2) (17) (2 3 3) (19) (2 2 5) (3 7) (2 11) (23) (2 2 2 3) (5 5) (2 13) (3 3 3) (2 2 7) (29) (2 3 5) (31) (2 2 2 2 2) (3 11) (2 17) (5 7) (2 2 3 3) (37) (2 19) (3 13) (2 2 2 5) (41) (2 3 7) (43) (2 2 11) (3 3 5) (2 23) (47) (2 2 2 2 3) (7 7) (2 5 5) (3 17) (2 2 13) (53) (2 3 3 3) (5 11) (2 2 2 7) (3 19) (2 29) (59) (2 2 3 5) (61) (2 31) (3 3 7) (2 2 2 2 2 2) (5 13) (2 3 11) (67) (2 2 17) (3 23) (2 5 7) (71) (2 2 2 3 3) (73) (2 37) (3 5 5) (2 2 19) (7 11) (2 3 13) (79) (2 2 2 2 5) (3 3 3 3) (2 41) (83) (2 2 3 7) (5 17) (2 43) (3 29) (2 2 2 11) (89) (2 3 3 5) (7 13) (2 2 23) (3 31) (2 47) (5 19) (2 2 2 2 2 3) (97) (2 7 7) (3 3 11)))

(= (map reverse (map factors (range 1 100))) '(() (2) (3) (2 2) (5) (3 2) (7) (2 2 2) (3 3) (5 2) (11) (3 2 2) (13) (7 2) (5 3) (2 2 2 2) (17) (3 3 2) (19) (5 2 2) (7 3) (11 2) (23) (3 2 2 2) (5 5) (13 2) (3 3 3) (7 2 2) (29) (5 3 2) (31) (2 2 2 2 2) (11 3) (17 2) (7 5) (3 3 2 2) (37) (19 2) (13 3) (5 2 2 2) (41) (7 3 2) (43) (11 2 2) (5 3 3) (23 2) (47) (3 2 2 2 2) (7 7) (5 5 2) (17 3) (13 2 2) (53) (3 3 3 2) (11 5) (7 2 2 2) (19 3) (29 2) (59) (5 3 2 2) (61) (31 2) (7 3 3) (2 2 2 2 2 2) (13 5) (11 3 2) (67) (17 2 2) (23 3) (7 5 2) (71) (3 3 2 2 2) (73) (37 2) (5 5 3) (19 2 2) (11 7) (13 3 2) (79) (5 2 2 2 2) (3 3 3 3) (41 2) (83) (7 3 2 2) (17 5) (43 2) (29 3) (11 2 2 2) (89) (5 3 3 2) (13 7) (23 2 2) (31 3) (47 2) (19 5) (3 2 2 2 2 2) (97) (7 7 2) (11 3 3)))

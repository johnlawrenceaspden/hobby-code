;; (filter #(integer? (/ 91653 %)) (range 1 300))

(defn factors [n]
  (loop [n n ,test 2, acc '(), sofar 1]
    (when (integer? (/ test 10000)) 
      (println n test acc sofar))
    (if (= n 1) 
      (reverse acc)
      (if (< n (* test test))
        (reverse (cons n acc))
        (if-not (integer? (/ n test))
          (recur n (inc test) acc sofar)
          (recur (/ n test) test (cons test acc) (* sofar test)))))))
      
(factors 1000)
;;(factors 9165379787907897767841097481074801)
(factors 9165379787902562387)

(* 2 13 7523)
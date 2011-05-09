;; probability of k zeros on n n sided dice

;; ie no of ones with 6d6, 8d8, 20d20

;; six d6

;;There are 
(reduce * (repeat 6 6)) ; 46656
;;ways for the dice to fall.

;; There's only one way you can get 6 ones

;; How many ways can you get 5?

;; Take the 6 arrangement, and take any dice and move it into any of its five other positions.

;; That's thirty different things, and each one leads to a different arrangement of the dice.

;; How many ways can you get no ones?

;; Each dice can be in one of five positions, so that's

(reduce * (repeat 6 5)) ; 7776

;; The answer is

(/ (reduce * (repeat 6 5.))
   (reduce * (repeat 6 6))) ; 15625/46656

;; For twenty d20
(/ (reduce * (repeat 20 19))
   (reduce * (repeat 20 20)))

;; Generally,

(defn probability-of-nowt [n]
  (reduce * (repeat n (/ (dec n) n))))

(require '[clojure.pprint :as cpp])
(cpp/cl-format nil "~{~$ ~}" (map probability-of-nowt (range 1 200 10)))

;; The probability of getting one zero is also easy to calculate.

;; Choose any of the n dice, and put it on 1, then there are (n-1)^(n-1) ways of arranging
;; all the other dice.

;; So there are n*(n-1)^(n-1) ways that the dice can fall so that only one one is showing
;; Out of n^n ways in total

(defn probability-of-one [n]
  (/ (* n (reduce * (repeat (dec n) (dec n))))
     (reduce * (repeat n n))))

(cpp/cl-format nil "~{~$ ~}" (map probability-of-one (range 1 200 10)))

;; Notice that the (probability-of-one n) = (/ (* n (probability-of-nowt (dec n))) (dec n))



     





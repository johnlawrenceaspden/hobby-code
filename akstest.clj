(def n 1023)

;; Is 1023 prime?

 ;; 48.031757700882885

(def needed-order (clojure.contrib.math/ceil (* (Math/log n) (Math/log n)))) ;; 49

;; We need to find an r such that the order of 1023 mod r is greater than 49

(* 1023)                         ;;1023
(* 1023 1023)                 ;;1046529
(* 1023 1023 1023)         ;;1070599167
(* 1023 1023 1023 1023) ;;1095222947841

;; Try 10
(mod 1023 10)                        ;3
(mod (* 1023 1023) 10)               ;9
(mod (* 1023 1023 1023) 10)          ;7
(mod (* 1023 1023 1023 1023) 10)     ;1

;; Therefore the order of 1023 mod 10 is 4 

;; We can generate sequences like that so
(take 10 (iterate #(mod (* % 1023) 10) (mod 1023 10)))

;; Because the powers are always less than r, whatever the first value is, 
;; we'll always eventually start to repeat.
(defn powers [n r]
  (take r (iterate #(mod (* % n) r) (mod n r))))

;; Sometimes we reach 1
(inf-powers 1023 10) ;; (3 9 7 1 3 9 7 1 3 9)
(inf-powers 5 7)     ;; (5 4 6 2 3 1 5)
;; Sometimes we don't
(inf-powers 100 10)  ;; (0 0 0 0 0 0 0 0 0 0)
(inf-powers 24 14)   ;; (10 2 6 4 12 8 10 2 6 4 12 8 10 2)
(inf-powers 18 24)   ;; (18 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(inf-powers 6 24)    ;; (6 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(inf-powers 3 12)    ;; (3 9 3 9 3 9 3 9 3 9 3 9)
(inf-powers 2 12)    ;; (2 4 8 4 8 4 8 4 8 4 8 4)

;; If n and r have a common factor, we can take it out of both of them
;; And get a sequence with the same period




(mod 24 14) ;10
(mod (* 24 24) 14) ;2
(mod (* 24 24 24) 14) ;6

(mod 12 7)        ;5
(mod (* 12 12) 7) ;4

(mod (* 2 2 12 12) (* 2 7))
(* 2 (mod (* 2 12 12) (* 7)))
(* 2 (mod (* (mod 2 7) (mod 12 7) (mod 12 7)) 7))



(inf-powers 10 1)   ;;(0)
(inf-powers 12 7)   ;;(5 4 6 2 3 1 5)

;; This will only get to 1 if n and r are coprime.
;; I think that's obvious, but I'm just guessing

(defn gcd [n r]
  (if (< n r) (gcd r n)
      (if (zero? (mod n r)) r
          (gcd (mod n r) r))))

(defn coprime? [n r] (= (gcd n r) 1))

(powers 1023 10)
(gcd 1023 10)

(powers 1023 9)
(gcd 

(defn order [n r]
  (if (zero? (mod n r)) (throw (new Exception (str "oops: mod " n " " r " = 0, so order is infinite")))
  (count (powers n r))))

(order 1023 10)
(mod (apply * (repeat (order 1023 10) 1023)) 10)

(take 10 (inf-powers 1023 3)) ;;(/ 1023 3)
(count (powers 1023 2))
(order 1023 2)
(mod (apply * (repeat (order 1023 2) 1023)) 2)

(map #(order 1023 %) (range 2 10))

(map (fn [r] (if (zero? (mod n r)) nil (order 1023 r))) '( 2 3 4 5 6))

(map (fn [r] (if (zero? (mod n r)) nil (order 1023 r))) (range 2 100))


 



(gcd 1023 6)
(mod 1023 6) -> 3
(gcd 3 6)
(gcd 6 3)
(mod 6 3) -> 3
(gcd 3 3)
(mod 6 3) -> 0
3

(gcd 1023 7)
(mod 1023 7) -> 1
(gcd 1 7)
(gcd 7 1)
(mod 7 1) -> 0
1


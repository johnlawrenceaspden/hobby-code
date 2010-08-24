;; Is 1023 prime?

;; 48.031757700882885

(defn needed-order [n]
  (int (clojure.contrib.math/ceil (* (Math/log n) (Math/log n)))))

(needed-order 1023)

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

;; Sometimes we reach 1 ; idempotence
(powers 1023 10) ;; (3 9 7 1 3 9 7 1 3 9)
(powers 5 7)     ;; (5 4 6 2 3 1 5)
(powers 3 4)     ;; (3 1 3 1)
(powers 12 7)    ;; (5 4 6 2 3 1 5)
(powers 9 7)     ;; (2 4 1 2 4 1 2)



;; Sometimes we reach 0 ; nilpotence
(powers 18 24)   ;; (18 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(powers 6 24)    ;; (6 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(powers 100 10)  ;; (0 0 0 0 0 0 0 0 0 0)
(powers 6 8)     ;; (6 4 0 0 0 0 0 0)
(powers 6 12)    ;; (6 0 0 0 0 0 0 0 0 0 0 0)

;; Sometimes we get stuck in some other cycle ; 
(powers 3 12)    ;; (3 9 3 9 3 9 3 9 3 9 3 9)
(powers 2 12)    ;; (2 4 8 4 8 4 8 4 8 4 8 4)
(powers 10 14)   ;; (10 2 6 4 12 8 10 2 6 4 12 8 10 2)
(powers 4 14)    ;; (4 2 8 4 2 8 4 2 8 4 2 8 4 2)

;; It seems that We will only get to 1 if n and r are coprime.
;; I think that's obvious, but I'm just guessing

(defn gcd [n r]
  (if (< n r) (gcd r n)
      (if (zero? (mod n r)) r
          (gcd (mod n r) r))))

(defn coprime? [n r] (= (gcd n r) 1))

(defn order [n r]
  (if-not (coprime? n r) 0
      (inc (count (take-while #(not (= 1 %)) (powers n r))))))

(defn sufficiently-large-r [n]
  (second (first 
           (drop-while #(< (first %) (needed-order n)) 
                       (partition 2 (interleave 
                                     (map #(order 1023 %) (iterate inc 2))
                                     (iterate inc 2)))))))

(sufficiently-large-r 1023) ;; 67 will do for 1023 

;; So now we have to check whether 1023 is a perfect power

(defn take-until [pred seq]
  (let [[h t] (split-with #(not (pred %)) seq)]
    (concat h (list (first t)))))

(defn find [pred seq]
  (cond (empty? seq) nil
        (pred (first seq)) (first seq)
        :else (find pred (rest seq))))

(take-until #(>= % 1023) (iterate #(* 2 %) 2))
(take-until #(>= % 1023) (iterate #(* 3 %) 3))
(take-until #(>= % 1023) (iterate #(* 4 %) 4))

(map (fn [a] (take-until #(>= % 1023) (iterate #(* a %) a))) (range 2 32))
;; This proves it isn't, but I'm not sure how to check that speedily

;; This also looks like a proof. Maybe it can be sped up.
(Math/pow 1023 1/2) (* 31 31) (* 32 32)
(Math/pow 1023 1/3) (* 10 10 10) (* 11 11 11)
(Math/pow 1023 1/4) (* 5 5 5 5) (* 6 6 6 6)
(Math/pow 1023 1/5) (* 4 4 4 4 4) (* 3 3 3 3 3)
(Math/pow 1023 1/6) (* 4 4 4 4 4 4) (* 3 3 3 3 3 3)
(Math/pow 1023 1/7) (* 2 2 2 2 2 2 2) (* 3 3 3 3 3 3 3)  
(Math/pow 1023 1/8) (* 2 2 2 2 2 2 2 2) (* 3 3 3 3 3 3 3 3)  


;; Let's try working out whether 1067597283596 is a perfect power:
(count (take-until #(>= % 1067597283596) (iterate #(* 2 %) 2)))
;; It's not a power of 2
( - 1067597283596 (apply * (repeat 40 2)))
( - 1067597283596 (apply * (repeat 39 2)))

;;Heron's method for square root
(clojure.contrib.math/ceil (/ (+ 1 (/ 1067597283596 1)) 2))
;; 533798641799
(clojure.contrib.math/ceil (/ (+ 533798641799 (/ 1067597283596 533798641799)) 2))
;; 266899320901
(clojure.contrib.math/ceil (/ (+ 266899320901 (/ 1067597283596 266899320901)) 2))
;; 133449660453
(defn heron [r]
  (clojure.contrib.math/ceil (/ (+ r (/ 1067597283596 r)) 2)))
(heron 1) (heron 533798641799)(heron 266899320901)
(last (take 30 (iterate heron 1)))

;;or alternatively
(Math/pow 1067597283596 1/2) 1033245.9937478587
;;candidates for square root are 1033245
(- 1067597283596 (* 1033245 1033245))
(- 1067597283596 (* 1033246 1033246))
;;so it's not a square
(Math/pow 1067597283596 1/3) ;;10220.429653736555
;;candidates for cube roots are 10220 10221 
( - 1067597283596 (apply * (repeat 3 10220)))
( - 1067597283596 (apply * (repeat 3 10221)))
;; so it's not a cube
(Math/pow 1067597283596 1/4) ;;1016.4870848898469
;;candidates for fourth roots are 1016 1017 
( - 1067597283596 (apply * (repeat 4 1016)))
( - 1067597283596 (apply * (repeat 4 1017)))
(Math/pow 1067597283596 1/5) ;;254.49631130975138
;;candidates for fifth roots are 254 255
( - 1067597283596 (apply * (repeat 5 254)))
( - 1067597283596 (apply * (repeat 5 255)))
(Math/pow 1067597283596 1/6) ;;101.0961406470919
;;candidates for sixth roots are 101 102
( - 1067597283596 (apply * (repeat 6 101)))
( - 1067597283596 (apply * (repeat 6 102)))
(Math/pow 1067597283596 1/7) ;;52.281004448701395
;;candidates for seventh roots are 52 53
( - 1067597283596 (apply * (repeat 7 52)))
( - 1067597283596 (apply * (repeat 7 53)))
(Math/pow 1067597283596 1/8) ;;31.882394591527262
;;candidates for eighth roots are 31 32
( - 1067597283596 (apply * (repeat 8 31)))
( - 1067597283596 (apply * (repeat 8 32)))
(Math/pow 1067597283596 1/9) ;;21.701498232144274
;;candidates for ninth roots are 21 22
( - 1067597283596 (apply * (repeat 9 21)))
( - 1067597283596 (apply * (repeat 9 22)))
;;and so now we need to show that no power of any number 2,3,4...21
;;is a root
(some zero? 
      ( mapcat (fn [a] 
                 (map #(- 1067597283596 %) 
                      (take-until #(>= % 1067597283596) 
                                  (iterate #(* a %) a)))) 
               (range 2 22)))




























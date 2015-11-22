;; Algorithms 101

;; Can anyone tell me what this fraction is in its simplest form?

;; 166022 / 249033

;; No? What do we need to know?

;; Well, we need to cast out a common factor. Can you see one?

(def a 166022) ; #'user/a
(def b 249033) ; #'user/b

( / a 2) ; 83011 
( / b 2) ; 249033/2

;; So 2 doesn't work...

( / a 3) ; 166022/3
( / b 3) ; 83011

;; Nor 3
;; Here's another way to check that

(rem a 3) ; 2  ; no remainder means a=3*something+2
(rem b 3) ; 0  ; no remainder means b=3*something

(defn common-factor? [n]
  (= (rem a n) (rem b n) 0))

(common-factor? 2) ; false
(common-factor? 3) ; false
(common-factor? 5) ; false
(common-factor? 7) ; false

(filter common-factor? '(2 3 5 7 11 13 17 19 23 27)) ; (17 19)

(/ a 17 19) ; 514
(/ b 17 19) ; 771

;; 514/771

;; Are we done yet?

(filter common-factor? (range 2 1000000)) ; (17 19 257 323 4369 4883 83011)

(/ a 83011) ; 2
(/ b 83011) ; 3

;; Ahh, so it was 2/3 all along.

(last (filter common-factor? (range 2 1000000))) ; 83011

(defn gcd [a b]
  (let [cf? (fn [n] (= 0 (rem a n) (rem b n)))]
    (last (filter cf? (range 1 (inc (max a b)))))))

(gcd 4 8) ; 4 ; 4 
(gcd 235 12) ; 1 ; 1

(gcd a b) ; 83011 ; 83011

(time (gcd a b)) ; 83011


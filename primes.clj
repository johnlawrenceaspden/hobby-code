;; This prime-finding code was originally posted by Rich Hickey, but it has
;; a mistake in it.

;;

(defn sieve [n]
  (let [n (int n)]
    "Returns a list of all primes from 2 to n"
    (let [root (int (Math/round (Math/floor (Math/sqrt n))))]
      (loop [i (int 3)
             a (int-array n)
             result (list 2)]
        (if (>= i n)
          (reverse result)
          (recur (+ i (int 2))
                 (if (< i root)  ;;There is a mistake here: should be <=
                   (loop [arr a
                          inc (+ i i)
                          j (* i i)]
                     (if (>= j n)
                       arr
                       (recur (do (aset arr j (int 1)) arr)
                              inc
                              (+ j inc))))
                   a)
                 (if (zero? (aget a i))
                   (conj result i)
                   result)))))))

(sieve 10)


;;To do:
(sieve 10) ;;(2 3 5 7 9) Sigh. Nine is not a prime. 
(sieve 12)
(sieve 15)
(sieve 16)
(sieve 24) ;; (2 3 5 7 11 13 17 19 23)
(sieve 25) ;; (2 3 5 7 11 13 17 19 23)
(sieve 35) ;; (2 3 5 7 11 13 17 19 23 25 29 31) ;;neither is 25!
(sieve 36) ;; (2 3 5 7 11 13 17 19 23 29 31)
(sieve 48) ;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
(sieve 49) ;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
(sieve 50) ;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 49)
(sieve 63) ;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59 61) ;; or 49
(sieve 64) ;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61)
(sieve 122);; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 121)
(sieve 143) ;;(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 121)
(sieve 1000)
(sieve 10000)
(take-last 10 (sieve 100000))
(take-last 10 (sieve 1000000))
(time (take-last 10 (sieve 1000000)))
(time (take-last 10 (sieve 10000000))) ;;5 seconds!


;; A more illuminating phrasing of this idea is:

(defn strikeout 
  "in the array a, which is n long, sets to 1 i*i, i*i+2*i, i*i+4*i, etc.
  a 1 -> 1, 3, 5, 7, 9..., a 2 -> 4,8,12,16.., a 3 -> 9,15,21..."
  [arr i n]
  (loop [inc (+ i i)
         j (* i i)]
    (if (>= j n)
      arr
      (do (aset arr j (int 1)) arr
          (recur 
             inc
             (+ j inc))))))

(seq (strikeout (int-array 20) 3 20))

(defn thingy [n]
  (loop [i (int 3)
         a (int-array n)
         result (list 2)]
    (if (>= i n)
      result
      (recur (+ i (int 2))
             (strikeout a i n)
             (if (zero? (aget a i))
               (conj result i)
               result)))))


;;Thingy can be expressed as (outer-thingy (inner-thingy n))

(defn inner-thingy [n]
  (let [a (int-array n)]
    (loop [i (int 3)]
      (when (< i n)
        (strikeout a i n)
        (recur (+ i (int 2)))))
    a))

;;Inner-thingy could also be expressed like
(defn inner-thingy [n]
  (let [a (int-array n)]
    (doseq [i (range 3 n 2)]
        (strikeout a i n))
    a))

(inner-thingy 20)
a (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
a (0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 1 0 0 1 0)
a (0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 1 1 0 1 0)


(seq (inner-thingy 30))

(defn outer-thingy [a n]
  "in an int-array a[n] return 2 plus all odd indices greater than 3 whose elements are 0"
  (loop [i 3
         result '(2)]
    (if (>= i n)
      result
      (recur (+ i 2)
             (if (zero? (aget a i))
               (conj result i)
               result))))))


(outer-thingy (int-array '(1 1 1 0 1 0 1 0 1 1)) 10)

(let [n 100
      root (int (Math/round (Math/floor (Math/sqrt n))))]
  (reverse (outer-thingy (inner-thingy n) n)))


;;real sieve is:

;;strike out all twos
a (0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
;;don't need to bother, just don't bother counting the evens, but add two back in
;;strike out all three
a (0 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0)
;; but don't strike 3, because it is a prime
;; and don't need to bother with 6, because any multiple of 3 less than 9
;; will have been crossed off earlier.











;; Information Theory, Inference, and Learning Algorithms
;; Exercise 1.3

;; A binary symmetric channel of noise f flips each bit with probability f

(defn flip[bit]
  (if (= bit 0) 1 0))

(defn corrupt[f sq]
  (lazy-seq
   (when-let [sq (seq sq)]
     (cons (if (< (rand) f) (flip (first sq)) (first sq))
           (corrupt f (rest sq))))))

;; (corrupt 1   (repeat 10 1))
;; (corrupt 0   (repeat 10 1))
;; (corrupt 0.3 (repeat 10 1))

(defn bsc[f] (fn[sq] (corrupt f sq)))

(def perfect      (bsc 0))
(def perfect-flip (bsc 1))
(def awful        (bsc 0.5))
(def slight-noise (bsc 0.01))
(def tenpercent-noise (bsc 0.1))

(map (fn [c] (c '(0 0 0 0 1 1 1 1)))
     (list perfect ,
           perfect-flip,
           awful,
           slight-noise,
           tenpercent-noise))

;; repetition codes just repeat each bit n times
(defn repetition-code [n sq]
  (lazy-seq
   (when-let [sq (seq sq)]
      (concat (repeat n (first sq)) (repetition-code n (rest sq))))))



(defn repetition-decode [n sq]
  (lazy-seq
   (when-let [sq (seq sq)]
     (let [ssq (take n sq)
           d (first (last (sort-by second (frequencies ssq))))]
       (cons d (repetition-decode n (drop n sq)))))))

(defn estimate-bit-error [n]
  (with-precision 15
    (bigdec
     (let [clength n
           tx (repeat 100000 (rand-int 2))
           rx (tenpercent-noise (repetition-code clength tx))
           drx (repetition-decode clength rx)
           ferrs (frequencies (map bit-xor tx drx))]
       (/ (get ferrs 1 0) (+ (get ferrs 0 0) (get ferrs 1 0)))))))



(estimate-bit-error 3) ; 0.02716M
(estimate-bit-error 5) ; 0.00827M
(estimate-bit-error 7) ; 0.00295M
(estimate-bit-error 9) ; 0.0008M

;; How shall we calculate the probability of bit-error for a repetition code?

;; For the 3-code, we get

;; f^3 times all three flip
;; 3f^2(1-f) two flip

;; So bit error is 3f^2-2f^3. For small f dominated by 3f^2

(defn calculate-bit-error [n f]
  (if (= n 3) (- (* 3 f f) (* 2 f f f))
      'dunno))

(calculate-bit-error 3 0.1) ;; 0.028000000000000004

;; For the five-code we need (1 3 3 1 ; 1 4 6 4 1 ; 1 5 10 10 5 1)

;; f^5+5f^4(1-f)+10f^3(1-f)^2

(defn factorial [n] (if (< n 2) 1N (* n (factorial (dec n)))))
(defn choose [ n r ]  (/ (factorial n) (factorial r) (factorial (- n r))))

;; (defn choose [n r] (reduce * (apply map / (list (sort (range n r -1)) (sort (range 1 (inc (- n r))))))))

(map #(choose 4 %) (range 7)) ;; (1N 4N 6N 4N 1N 1/5 1/30)

(defn bit-error-terms [n f]
  (map (fn[r] (* (choose n r) (power f r) (power (- 1 f) (- n r)))) (range (/ (+ 1 n) 2) (+ 1 n))))

(bit-error-terms 3 0.1) ; (0.027000000000000007 0.0010000000000000002)
(bit-error-terms 5 0.1) ; (0.008100000000000001 4.500000000000001E-4 1.0000000000000004E-5)
(bit-error-terms 7 0.1) ; (0.002551500000000001 1.701000000000001E-4 6.300000000000002E-6 1.0000000000000005E-7)

(defn calculate-bit-error [n f]
  (reduce + (bit-error-terms n f))

(calculate-bit-error 3 0.1) ; 0.028000000000000008
(calculate-bit-error 5 0.1) ; 0.008560000000000002
(calculate-bit-error 7 0.1) ; 0.002728000000000001
(calculate-bit-error 9 0.1) ; 8.909200000000006E-4

(take 60 (map #(calculate-bit-error % 0.1) (range 3 100 2)))

;; Calculate-bit-error will be dominated by a term
;; (choose n r) f^r (1-f)^(n-r) with r= n+1/2

(defn rough-bit-error [n f]
  (let [r (/ (+ n 1) 2)]
    (* (choose n r) (power f r) (power (- 1 f) (- n r)))))


(take 60 (map #(/(calculate-bit-error % 0.1) (rough-bit-error % 0.1)) (range 3 100 2)))

;; The second largest term will be
;; (choose n (inc r)) f^(inc r) (1-f)^(n-(inc r)
;; which you get by multiplying the leading term by 0.1 0.9 (/ (n-1) (n+3))
(map (fn [[a b]] (/ a b)) (map (juxt first second) (map #(bit-error-terms % 0.1) (range 3 200 2))))


;; In fact we can replace (choose n r) with stirling's approximation (power x x) (exp -x)
(defn rough-choose [n r] (/ (power n n) (power r r) (power (- n r) (- n r))))

;; Which is only quite crap
(defn stirling-wrongness [n]
  (reduce max (map (fn[r] (float (/ (rough-choose n r) (choose n r)))) (range (inc n)))))

(stirling-wrongness 21) ; 5.805778980255127
(stirling-wrongness 50) ; 8.906688690185547
(stirling-wrongness 100) ; 12.564513206481934
(stirling-wrongness 200) ; 17.746707916259766

;; So we can make this approximation, knowing that we're going to be out by an order of magnitude or so
(defn very-rough-bit-error [n f]
  (let [r (/ (+ n 1) 2)
        nr (/ (- n 1) 2)]
    (* (/ (power n n) (power r r) (power nr nr))
       (power f r) (power (- 1 f) nr))))

(very-rough-bit-error 3 0.1) ; 0.06075000000000002
(very-rough-bit-error 5 0.1) ; 0.02343750000000001
(very-rough-bit-error 7 0.1) ; 0.008685805078125003
(very-rough-bit-error 9 0.1) ; 0.0031773322854112517


(defn rearranged-very-rough-bit-error [n f]
  (let [r (/ (+ n 1) 2)
        nr (/ (- n 1) 2)
        a (* f (+ 1 (/ nr r)))
        b (* (- 1 f) (+ (/ r nr) 1))]
     (* (power a r) (power b nr) )))

(rearranged-very-rough-bit-error 3 0.1) ; 0.06075000000000002
(rearranged-very-rough-bit-error 5 0.1) ; 0.02343750000000002
(rearranged-very-rough-bit-error 7 0.1) ; 0.008685805078124999
(rearranged-very-rough-bit-error 9 0.1) ; 0.0031773322854112503
(rearranged-very-rough-bit-error 11 0.1) ; 0.00115551226597455

;; If we're willing to make a few more approximations
(defn dumbass-bit-error [n f]
  (let [a (* f 2)
        b (* (- 1 f) 2)]
    (* a (power (* a b) (/ (- n 1) 2)) )))

;; For f = 0.1 something like
;; 0.2 * (power 0.36 ((n-1)/2)), implying 60 repeats gives you about 15 zeros.

;; So how much repetition-coding do we need to get an error rate of 10^-15?
;; Real answer
(calculate-bit-error 59 0.1) ; 3.105649572055603E-15
(calculate-bit-error 61 0.1) ; 1.1003005338682957E-15

;; Answer by approximation (out by 4)
(rearranged-very-rough-bit-error 59 0.1) ; 2.6922902595519587E-14
(rearranged-very-rough-bit-error 61 0.1) ; 9.694938721333437E-15
(rearranged-very-rough-bit-error 63 0.1) ; 3.4910863670869607E-15
(rearranged-very-rough-bit-error 65 0.1) ; 1.2570980758206157E-15

(dumbass-bit-error 59 0.1) ; 2.7152043322605233E-14
(dumbass-bit-error 61 0.1) ; 9.774735596137885E-15
(dumbass-bit-error 63 0.1) ; 3.5189048146096392E-15
(dumbass-bit-error 65 0.1) ; 1.2668057332594703E-15




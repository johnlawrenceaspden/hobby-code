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

;; repetition codes just repeat each bit three times
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

(with-precision 10
  (bigdec
   (let [clength 64
         tx (repeat 10000 (rand-int 2))
         rx (tenpercent-noise (repetition-code clength tx))
         drx (repetition-decode clength rx)
         ferrs (frequencies (map bit-xor tx drx))]
     (/ (get ferrs 1 0) (+ (get ferrs 0 0) (get ferrs 1 0))))))













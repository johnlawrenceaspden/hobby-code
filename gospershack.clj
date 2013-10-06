;; unsigned nexthi_same_count_ones(unsigned a) {
;;   /* works for any word length */
;;   unsigned c = (a & -a);
;;   unsigned r = a+c;
;;   return (((r ^ a) >> 2) / c) | r);
;; }

(defn gosper [a]
  (let [_ (println "a=" (Long/toBinaryString a))
        b (dbg (unchecked-subtract 0 a)) ;1111111111
        _ (println "b=" (Long/toBinaryString b))
        c (dbg (bit-and a b)) ; 1
        _ (println "c (least significant bit) =" (Long/toBinaryString c))
        r (dbg (unchecked-add a c))    ;10 
        _ (println "r=" (Long/toBinaryString r))
        s (dbg (bit-xor r a))          ;11
        _ (println "s=" (Long/toBinaryString s))
        t (dbg (bit-shift-right s 2))   ;0
        _ (println "t=" (Long/toBinaryString t))
        u (dbg (quot t c))                 ;0
        _ (println "u="(Long/toBinaryString u))
        v (dbg (bit-or u r))]          ;10
    v))

(Integer/toBinaryString (gosper 2r00001)) ;->     "10"
(Integer/toBinaryString (gosper 2r00010)) ;->    "100"
(Integer/toBinaryString (gosper 2r00100)) ;->   "1000"
(Integer/toBinaryString (gosper 2r01000)) ;->  "10000"
(Integer/toBinaryString (gosper 2r10000)) ;-> "100000"

(Integer/toBinaryString (gosper 2r00011)) ;->    "101"
(Integer/toBinaryString (gosper 2r00101)) ;->    "110"
(Integer/toBinaryString (gosper 2r00110)) ;->   "1001"
(Integer/toBinaryString (gosper 2r01001)) ;->   "1010"
(Integer/toBinaryString (gosper 2r01010)) ;->   "1100"
(Integer/toBinaryString (gosper 2r01100)) ;->  "10001"
(Integer/toBinaryString (gosper 2r10001)) ;->  "10010"
(Integer/toBinaryString (gosper 2r10010)) ;->  "10100"
(Integer/toBinaryString (gosper 2r10100)) ;->  "11000"

(defn gosper [a]
  (let [c (bit-and a (unchecked-subtract 0 a))
        r (unchecked-add a c)]
    (bit-or (quot (bit-shift-right (bit-xor r a) 2) c) r)))


(map #(Integer/toBinaryString %) (iterate gosper 1))  ;-> ("1" "10" "100" "1000" "10000" "100000" "1000000" "10000000" "100000000" "1000000000" "10000000000" "100000000000" "1000000000000" "10000000000000" "100000000000000" "1000000000000000" "10000000000000000" "100000000000000000" "1000000000000000000" "10000000000000000000" "100000000000000000000" "1000000000000000000000" "10000000000000000000000" "100000000000000000000000" "1000000000000000000000000" "10000000000000000000000000" "100000000000000000000000000" ...)
(map #(Integer/toBinaryString %) (iterate gosper 3))  ;-> ("11" "101" "110" "1001" "1010" "1100" "10001" "10010" "10100" "11000" "100001" "100010" "100100" "101000" "110000" "1000001" "1000010" "1000100" "1001000" "1010000" "1100000" "10000001" "10000010" "10000100" "10001000" "10010000" "10100000" ...)
(map #(Integer/toBinaryString %) (iterate gosper 7))  ;-> ("111" "1011" "1101" "1110" "10011" "10101" "10110" "11001" "11010" "11100" "100011" "100101" "100110" "101001" "101010" "101100" "110001" "110010" "110100" "111000" "1000011" "1000101" "1000110" "1001001" "1001010" "1001100" "1010001" ...)
(map #(Integer/toBinaryString %) (iterate gosper 15)) ;-> ("1111" "10111" "11011" "11101" "11110" "100111" "101011" "101101" "101110" "110011" "110101" "110110" "111001" "111010" "111100" "1000111" "1001011" "1001101" "1001110" "1010011" "1010101" "1010110" "1011001" "1011010" "1011100" "1100011" "1100101" ...)


(def comb3 (for [a (range) b (range 0 a) c (range 0 b)] [a b c]))

(reduce + (map #(bit-shift-left 1 %) [2 1 0]))

(for [a (range) b (range 0 a) c (range 0 b)] 
  (reduce + (map #(bit-shift-left 1 %) [a b c])))

(iterate gosper 7)


(for [a (range) b (range 0 a) c (range 0 b) d (range 0 c)] 
  (reduce + (map #(bit-shift-left 1 %) [a b c d]))) 
; (15 23 27 29 30 39 43 45 46 51 53 54 57 58 60 71 75 77 78 83 85 86 89 90 92 99 101 ...)

(iterate gosper 15) 
;-> (15 23 27 29 30 39 43 45 46 51 53 54 57 58 60 71 75 77 78 83 85 86 89 90 92 99 101 ...)

(iterate gosper 31) ;-> (31 47 55 59 61 62 79 87 91 93 94 103 107 109 110 115 117 118 121 122 124 143 151 155 157 158 167 ...)


(defn subsets[pattern]
  (filter identity 
          (for [i (range 64)] 
            (if (zero? (bit-and pattern (bit-shift-left 1 i)))
              nil
              [i (bit-xor pattern (bit-shift-left 1 i))]))))

(subsets 1) ;-> ([0 0])
(subsets 2) ;-> ([1 0])
(subsets 3) ;-> ([0 2] [1 1])
(subsets 4) ;-> ([2 0])
(subsets 5) ;-> ([0 4] [2 1])
(subsets 6) ;-> ([1 4] [2 2])
(subsets 7) ;-> ([0 6] [1 5] [2 3])

(map subsets (iterate gosper 2r111))

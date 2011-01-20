(defn tosses[]
  (let [p (rand)]
    (cons p (map (fn[_](if (< (rand) p) :h :t)) (range)))))

(def coins (for [i (range)] (tosses)))

(defn select-tosses [sq]
  (filter #(= (take (count sq) (drop 1 %)) sq) coins))

(defn select-coins [sq]
  (map first (select-tosses sq)))

(def select-coins (memoize select-coins))

(defn next-vals [sq]
  (map #(nth % (inc (count sq))) (select-tosses sq)))

(defn laplace-rule [sq]
  (let [f (frequencies sq)]
      (str (inc (get f :h 0)) ":" (inc (get f :t 0)))))

(defn laplace-by-sampling [sq n]
  (let [f (frequencies (take n (next-vals sq)))]
    (str (get f :h 0) ":" (get f :t 0))))

(laplace-rule '(:h :h))
(laplace-by-sampling '(:h :h) 40000)

(defn rangen[n] (map * (repeat (/ 1 n)) (range (+ n 1))))
(defn intervals [n] (partition 2 1 (rangen 10.0)))

(defn between [samples]
  (let [st (sort samples)]
    (fn [a b] (count (take-while #(< % b) (drop-while #(< % a) st))))))

(defn histogram [sq samplesize bins]
  (map (fn [[a b]] ((between (take samplesize (select-coins sq))) a b)) (intervals bins)))

(histogram '()   10000 10) ; (972 971 1053 987 1000 1015 975 1013 1016 998)

(histogram '(:h) 10000 10) ; (126 292 516 654 835 1152 1297 1526 1715 1887)
(histogram '(:t) 10000 10) ; (1820 1653 1580 1295 1123 913 663 547 310 96)

(map + 
     (histogram '(:t :t) 10000 10) ; (2696 2181 1722 1262 906 649 326 167 81 10)
     (histogram '(:t :h) 5000 10) ; (129 373 587 669 786 705 679 582 355 135)
     (histogram '(:h :t) 5000 10) ; (160 350 565 634 694 767 699 620 363 148)
     (histogram '(:h :h) 10000 10) ; (12 80 190 376 564 961 1234 1683 2175 2725)
     ) ; (2997 2984 3064 2941 2950 3082 2938 3052 2974 3018)




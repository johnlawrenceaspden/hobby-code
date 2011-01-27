;; From the Joy of Clojure

(map [:chthon :phthor :beowulf :grendel] #{0 3}) ; (:chthon :grendel)

(def fifth (comp first rest rest rest rest))

(fifth [1 2 3 4 5]) ; 5

(defn fnth [n]
     (apply comp
            (cons first
                  (take (dec n) (repeat rest)))))

((fnth 5) '[a b c d e f]) ; e

(map (comp keyword #(.toLowerCase %) name) '(a B C abracadaBra)) ; (:a :b :c :abracadabra)

((partial + 5) 100 200) ; 305

(map (partial + 5) (range)) ; (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 ...)

(map #(+ 5 %) (range)) ; (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 ...)

((partial + 5) 100 200) ; 305

(#(apply + 5 %&) 100 200) ; 305

(let [truthiness (fn[v] v)]
  [((complement truthiness) true)
   ((complement truthiness) 42)
   ((complement truthiness) false)
   ((complement truthiness) nil)]) ; [false false true true]

((complement even?) 2) ; false

((comp not even?) 2) ; false

(defn join
  {:test (fn[]
           (assert
            (= (join "," [1 2 3]) "1,3,3")))}
  [sep s]
  (apply str (interpose sep s)))

(use '[clojure.test :as t])

(t/run-tests) ; {:type :summary, :pass 0, :test 1, :error 1, :fail 0}

(defrecord bpl [band plays loved])

(def plays [{:band "Burial", :plays 979, :loved 9}
            (bpl. "Eno" 2333 15)
            (bpl. "Bill Evans" 979 9)
            (bpl. "Magma" 2665 31)])

(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))

(sort-by-loved-ratio plays)

(defn columns [names]
  (fn [a]
    (vec (map #( % a) names))))

(sort-by (columns [:plays :loved :band]) plays) ;
(#:user.bpl{:band "Bill Evans", :plays 979, :loved 9}
           {:band "Burial", :loved 9, :plays 979}
           #:user.bpl{:band "Eno", :plays 2333, :loved 15}
           #:user.bpl{:band "Magma", :plays 2665, :loved 31})


(map {:band "Burial", :plays 979, :loved 9} [:band :plays :loved]) ; ("Burial" 979 9)
(map (bpl. "Burial" 979 9) [:band :plays :loved])

(select-keys '{c 3 b 2 a 1} '( c b)) ; {b 2, c 3}

(defn mapmap [f m]
  (zipmap (keys m) (map f (vals m))))

(mapmap (partial * 2) '{a 1 b 2 c 3}) ; {c 6, b 4, a 2}

(defn keys-apply [f ks m]
  (let [only (select-keys m ks)]
    (zipmap (keys only) (map f (vals only)))))

(keys-apply #(.toUpperCase %) #{:band} (plays 1))

(keys-apply (partial * 2) '(a b) '{a 1, b 2, c 3}) ; {a 2, b 4}

(keys-apply (partial * 2 2 2) '(a b) (zipmap '(a b c) [1 2 3])) ; {a 8, b 16}

(zipmap '(a b c) '(1 2 3)) ; {c 3, b 2, a 1}

(apply sorted-map (interleave '(a b c) '(1 2 3))) ; {a 1, b 2, c 3}
(apply hash-map (interleave '(a b c) '(1 2 3))) ; {a 1, c 3, b 2}

(defn manip-map [f ks m]
  (conj m (keys-apply f ks m)))

(conj '{a 1 b 2 c 3} '{a 2 b 4}) ; {a 2, b 4, c 3}
(conj '{a 2 b 4} '{a 1 b 2 c 3}) ; {c 3, a 1, b 2}

(conj [1 2] [3 4]) ; [1 2 [3 4]]
(conj '(1 2) '(3 4)) ; ((3 4) 1 2)

(conj [1 2] 1 2 3 4 5) ; [1 2 1 2 3 4 5]

(conj '{a 1 b 2} '[c 3]) ; {c 3, a 1, b 2}
(conj '{a 1 b 2} '{c 3}) ; {c 3, a 1, b 2}

(manip-map (comp int #( / % 2)) #{:plays :loved} (plays 0)) ; {:band "Burial", :loved 4, :plays 489}

(map (partial manip-map (comp int #( / % 2)) #{:plays :loved}) plays)
                                        ; ({:band "Burial", :loved 4, :plays 489}
                                        ; #:user.bpl{:band "Eno", :plays 1166, :loved 7}
                                        ; #:user.bpl{:band "Bill Evans", :plays 489, :loved 4}
                                        ; #:user.bpl{:band "Magma", :plays 1332, :loved 15})

(defn halve! [ks]
  (map (partial manip-map #(int (/ % 2)) ks) plays))

(halve! [:plays]) ; ({:band "Burial", :loved 9, :plays 489} #:user.bpl{:band "Eno", :plays 1166, :loved 15} #:user.bpl{:band "Bill Evans", :plays 489, :loved 9} #:user.bpl{:band "Magma", :plays 1332, :loved 31})

(halve! [:plays :loved]) ; ({:band "Burial", :loved 4, :plays 489} #:user.bpl{:band "Eno", :plays 1166, :loved 7} #:user.bpl{:band "Bill Evans", :plays 489, :loved 4} #:user.bpl{:band "Magma", :plays 1332, :loved 15})

(defn slope [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(slope :p1 [4 15]) ; 4.666666507720947
(slope :p2 [4 15]) ; 3.75
(slope :p1 [0 1] :p2 [1 1]) ; 0.0
(slope) ; 1.0

(defn slope [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

(defmacro t[& body]
  `(try ~@body
       (catch Throwable t# t#)))

(t (slope [1 0] [2 1.0])) ; 1.0
(t (slope [1 0] [2 1])) ; #<AssertionError java.lang.AssertionError: Assert failed: (float? %)>
(t (slope [1 0] '(2 1))) ; #<AssertionError java.lang.AssertionError: Assert failed: (vector? p2)>
(t (slope [1 0] [1 0])) ; #<AssertionError java.lang.AssertionError: Assert failed: (not= p1 p2)>


(defn put-things [m]
  (into m {:meat "beef" :veggie "brocolli"}))

(put-things {}) ; {:veggie "brocolli", :meat "beef"}
(put-things []) ; [[:veggie "brocolli"] [:meat "beef"]]
(put-things '()) ; ([:meat "beef"] [:veggie "brocolli"])
(put-things #{}) ; #{[:meat "beef"] [:veggie "brocolli"]}
(put-things #{:a :b :c}) ; #{:a :c [:meat "beef"] :b [:veggie "brocolli"]}
(put-things {:veggie "carrot"}) ; {:veggie "brocolli", :meat "beef"}

(defn vegan-constraints [f m]
  {:pre [(:veggie m)]
   :post [(:veggie %) (nil? (:meat %))]}
  (f m))

(t (vegan-constraints put-things {:veggie "carrot"})) ; #<AssertionError java.lang.AssertionError: Assert failed: (nil? (:meat %))>

(defn balanced-diet [f m]
  {:post [(:meat %) (:veggie %)]}
  (f m))

(balanced-diet put-things {}) ; {:veggie "brocolli", :meat "beef"}

(defn finicky [f m]
  {:post [(= (:meat %) (:meat m))]}
  (f m))

(t (finicky put-things {})) ; #<AssertionError java.lang.AssertionError: Assert failed: (= (:meat %) (:meat m))>
(t (finicky put-things {:meat "beef"})) ; {:meat "beef", :veggie "brocolli"}
(t (finicky put-things {:meat "venison"})) ; #<AssertionError java.lang.AssertionError: Assert failed: (= (:meat %) (:meat m))>

(def times-two
     (let [x 2]
       (fn [y] (* y x))))

(times-two 5) ; 10

(def times-2 (partial * 2))

(times-2 5) ; 10

(def add-and-get
     (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
       (fn[y] (.addAndGet ai y))))

(add-and-get 2) ; 2
(add-and-get 2) ; 4
(add-and-get 7) ; 11

(def add-and-get-squares
     (let [a (atom 0)]
       (fn[y] (swap! a + (* y y))))) ; #'user/add-and-get-squares

(add-and-get-squares 1) ; 1
(add-and-get-squares 2) ; 5
(add-and-get-squares 3) ; 14

(defn times-n [n]
  (let [x n]
    (fn [y] (* y x))))

(let [f (times-n 5)]
  [(f 3)(f 4) (f 6)]) ; [15 20 30]

(defn divisible [denom]
  (fn[num] (zero? (rem num denom))))

(filter (divisible 3) (range)) ; (0 3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 303 306 ...)

(filter (divisible 7) (filter (divisible 3) (range))) ; (0 21 42 63 84 105 126 147 168 189 210 231 252 273 294 315 336 357 378 399 420 441 462 483 504 525 546 567 588 609 630 651 672 693 714 735 756 777 798 819 840 861 882 903 924 945 966 987 1008 1029 1050 1071 1092 1113 1134 1155 1176 1197 1218 1239 1260 1281 1302 1323 1344 1365 1386 1407 1428 1449 1470 1491 1512 1533 1554 1575 1596 1617 1638 1659 1680 1701 1722 1743 1764 1785 1806 1827 1848 1869 1890q 1911 1932 1953 1974 1995 2016 2037 2058 2079 2100 2121 2142 ...)

(map #((let [x (juxt ((divisible 3) %) ((divisible 7) %))] (case x [true, true] "fizzbuzz" [true, false] "fizz" [false, true] "buzz" [false, false] %))) (range))



(map (juxt (divisible 3) (divisible 5)) (range))




(defn word-digit [n]
  (case n 0 "zero" 1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 6 "six" 7 "seven" 8 "eight" 9 "nine"
         10 "ten" 11 "eleven" 12 "twelve" 13 "thirteen" 14 "fourteen" 15 "fifteen" 16 "sixteen" 17 "seventeen" 18 "eighteen" 19 "nineteen"))

(map word-digit (range 1 20)) ; ("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen")

(defn word-tens [n]
  (case n 2 "twenty" 3 "thirty" 4 "forty" 5 "fifty" 6 "sixty" 7 "seventy" 8 "eighty" 9 "ninety"))


(defn word [n]
  (cond (<= 0 n 19) (word-digit n)
        (<= 20 n 99) (let [ y (quot n 10) x (rem n 10)] (if (= x 0)
                                           (word-tens y)
                                           (str (word-tens y) "-" (word-digit x))))
        :else (str n)))

(map word (range)) ; ("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen" "twenty" "twenty-one" "twenty-two" "twenty-three" "twenty-four" "twenty-five" "twenty-six" "twenty-seven" "twenty-eight" "twenty-nine" "thirty" "thirty-one" "thirty-two" "thirty-three" "thirty-four" "thirty-five" "thirty-six" "thirty-seven" "thirty-eight" "thirty-nine" "forty" "forty-one" "forty-two" "forty-three" "forty-four" "forty-five" "forty-six" "forty-seven" "forty-eight" "forty-nine" "fifty" "fifty-one" "fifty-two" "fifty-three" "fifty-four" "fifty-five" "fifty-six" "fifty-seven" "fifty-eight" "fifty-nine" "sixty" "sixty-one" "sixty-two" "sixty-three" "sixty-four" "sixty-five" "sixty-six" "sixty-seven" "sixty-eight" "sixty-nine" "seventy" "seventy-one" "seventy-two" "seventy-three" "seventy-four" "seventy-five" "seventy-six" "seventy-seven" "seventy-eight" "seventy-nine" "eighty" "eighty-one" "eighty-two" "eighty-three" "eighty-four" "eighty-five" "eighty-six" "eighty-seven" "eighty-eight" "eighty-nine" "ninety" "ninety-one" "ninety-two" "ninety-three" "ninety-four" "ninety-five" "ninety-six" "ninety-seven" "ninety-eight" "ninety-nine" "100" "101" "102" ...)
  
(defn divisible [denom]
  (fn[num] (zero? (rem num denom))))

(defn nsound [m s]
  (fn [[n str]] [n (if ((divisible m) n) (concat s str) str)]))

(map (comp
      (fn [[n s]] (if (empty? s) (word n) (apply str s)))
      (nsound 7 "sheesh")
      (nsound 3 "fizz")
      (nsound 5 "buzz")
      (fn [n][n ""])
      inc) (range)) ; ("one" "two" "fizz" "four" "buzz" "fizz" "sheesh" "eight" "fizz" "buzz" "eleven" "fizz" "thirteen" "sheesh" "fizzbuzz" "sixteen" "seventeen" "fizz" "nineteen" "buzz" "sheeshfizz" "twenty-two" "twenty-three" "fizz" "buzz" "twenty-six" "fizz" "sheesh" "twenty-nine" "fizzbuzz" "thirty-one" "thirty-two" "fizz" "thirty-four" "sheeshbuzz" "fizz" "thirty-seven" "thirty-eight" "fizz" "buzz" "forty-one" "sheeshfizz" "forty-three" "forty-four" "fizzbuzz" "forty-six" "forty-seven" "fizz" "sheesh" "buzz" "fizz" "fifty-two" "fifty-three" "fizz" "buzz" "sheesh" "fizz" "fifty-eight" "fifty-nine" "fizzbuzz" "sixty-one" "sixty-two" "sheeshfizz" "sixty-four" "buzz" "fizz" "sixty-seven" "sixty-eight" "fizz" "sheeshbuzz" "seventy-one" "fizz" "seventy-three" "seventy-four" "fizzbuzz" "seventy-six" "sheesh" "fizz" "seventy-nine" "buzz" "fizz" "eighty-two" "eighty-three" "sheeshfizz" "buzz" "eighty-six" "fizz" "eighty-eight" "eighty-nine" "fizzbuzz" "sheesh" "ninety-two" "fizz" "ninety-four" "buzz" "fizz" "ninety-seven" "sheesh" "fizz" "buzz" "101" "fizz" "103" ...)












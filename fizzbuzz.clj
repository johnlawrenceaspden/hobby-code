(defn word-digit [n]
  (case n 0 "zero" 1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 6 "six" 7 "seven" 8 "eight" 9 "nine"
         10 "ten" 11 "eleven" 12 "twelve" 13 "thirteen" 14 "fourteen" 15 "fifteen" 16 "sixteen" 17 "seventeen" 18 "eighteen" 19 "nineteen"))

(map word-digit (range 1 20)) ; ("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen")

(defn word-tens [n]
  (case n 2 "twenty" 3 "thirty" 4 "forty" 5 "fifty" 6 "sixty" 7 "seventy" 8 "eighty" 9 "ninety"))


(defn word-up-to-hundred [n]
  (cond (<= 0 n 19) (word-digit n)
        (<= 20 n 99) (let [ y (quot n 10) x (rem n 10)] (if (= x 0)
                                           (word-tens y)
                                           (str (word-tens y) "-" (word-digit x))))
        :else (str n)))

(defn word-up-to-thousand [n]
  (cond (< n 100) (word-up-to-hundred n)
        (< n 1000) (let [ y (quot n 100) x (rem n 100) ]
                     (if (= x 0) (str (word-digit y) " hundred")
                         (str (word-digit y) " hundred" " and " (word-up-to-hundred x))))))




(defn groups [n]
  (if (< n 1000) (list n)
      (let [y (quot n 1000) x (rem n 1000)]
        (cons x (groups y)))))

(defn word [n]
  (cond (= n 0) "zero"
        (< n 0) (str "minus " (word (- n)))
        :else (clojure.string/join " "
                                   (butlast 
                                    (apply concat 
                                           (map (fn[[a b]] [(word-up-to-thousand a) b])
                                                (reverse
                                                 (filter #(not (= 0 (first %))) (partition 2 (interleave (groups n)  '("","thousand," "million," "billion," "trillion," "quadrillion," "quintillion," "sextillion")))))))))))

(map word (iterate inc -50))
(map word (range))
(map word (take 10 (iterate inc 189610009999999999))) ;; bugger



(def word word-up-to-one-million)

(map word (range)) ; ("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen" "twenty" "twenty-one" "twenty-two" "twenty-three" "twenty-four" "twenty-five" "twenty-six" "twenty-seven" "twenty-eight" "twenty-nine" "thirty" "thirty-one" "thirty-two" "thirty-three" "thirty-four" "thirty-five" "thirty-six" "thirty-seven" "thirty-eight" "thirty-nine" "forty" "forty-one" "forty-two" "forty-three" "forty-four" "forty-five" "forty-six" "forty-seven" "forty-eight" "forty-nine" "fifty" "fifty-one" "fifty-two" "fifty-three" "fifty-four" "fifty-five" "fifty-six" "fifty-seven" "fifty-eight" "fifty-nine" "sixty" "sixty-one" "sixty-two" "sixty-three" "sixty-four" "sixty-five" "sixty-six" "sixty-seven" "sixty-eight" "sixty-nine" "seventy" "seventy-one" "seventy-two" "seventy-three" "seventy-four" "seventy-five" "seventy-six" "seventy-seven" "seventy-eight" "seventy-nine" "eighty" "eighty-one" "eighty-two" "eighty-three" "eighty-four" "eighty-five" "eighty-six" "eighty-seven" "eighty-eight" "eighty-nine" "ninety" "ninety-one" "ninety-two" "ninety-three" "ninety-four" "ninety-five" "ninety-six" "ninety-seven" "ninety-eight" "ninety-nine" "one hundred" "one hundred and one" "one hundred and two" ...)


(take 10 (drop 35246 (map word (range)))) ; ("thirty-five thousand two hundred and forty-six" "thirty-five thousand two hundred and forty-seven" "thirty-five thousand two hundred and forty-eight" "thirty-five thousand two hundred and forty-nine" "thirty-five thousand two hundred and fifty" "thirty-five thousand two hundred and fifty-one" "thirty-five thousand two hundred and fifty-two" "thirty-five thousand two hundred and fifty-three" "thirty-five thousand two hundred and fifty-four" "thirty-five thousand two hundred and fifty-five")




  
(defn divisible [denom]
  (fn[num] (zero? (rem num denom))))

(defn nsound [m s]
  (fn [[n str]] [n (if ((divisible m) n) (concat s str) str)]))

(def fizzbuzz (partial map (comp
      (fn [[n s]] (if (empty? s) (word n) (apply str s)))
      (nsound 7 "sheesh")
      (nsound 3 "fizz")
      (nsound 5 "buzz")
      (fn [n][n ""])
      inc)))

(fizzbuzz  (range)) ; ("one" "two" "fizz" "four" "buzz" "fizz" "sheesh" "eight" "fizz" "buzz" "eleven" "fizz" "thirteen" "sheesh" "fizzbuzz" "sixteen" "seventeen" "fizz" "nineteen" "buzz" "sheeshfizz" "twenty-two" "twenty-three" "fizz" "buzz" "twenty-six" "fizz" "sheesh" "twenty-nine" "fizzbuzz" "thirty-one" "thirty-two" "fizz" "thirty-four" "sheeshbuzz" "fizz" "thirty-seven" "thirty-eight" "fizz" "buzz" "forty-one" "sheeshfizz" "forty-three" "forty-four" "fizzbuzz" "forty-six" "forty-seven" "fizz" "sheesh" "buzz" "fizz" "fifty-two" "fifty-three" "fizz" "buzz" "sheesh" "fizz" "fifty-eight" "fifty-nine" "fizzbuzz" "sixty-one" "sixty-two" "sheeshfizz" "sixty-four" "buzz" "fizz" "sixty-seven" "sixty-eight" "fizz" "sheeshbuzz" "seventy-one" "fizz" "seventy-three" "seventy-four" "fizzbuzz" "seventy-six" "sheesh" "fizz" "seventy-nine" "buzz" "fizz" "eighty-two" "eighty-three" "sheeshfizz" "buzz" "eighty-six" "fizz" "eighty-eight" "eighty-nine" "fizzbuzz" "sheesh" "ninety-two" "fizz" "ninety-four" "buzz" "fizz" "ninety-seven" "sheesh" "fizz" "buzz" "one hundred and one" "fizz" "one hundred and three" ...)

(take 100 (drop 761524 (fizzbuzz (range)))) ; ("buzz" "fizz" "seven hundred and sixty-one thousand five hundred and twenty-seven" "seven hundred and sixty-one thousand five hundred and twenty-eight" "fizz" "sheeshbuzz" "seven hundred and sixty-one thousand five hundred and thirty-one" "fizz" "seven hundred and sixty-one thousand five hundred and thirty-three" "seven hundred and sixty-one thousand five hundred and thirty-four" "fizzbuzz" "seven hundred and sixty-one thousand five hundred and thirty-six" "sheesh" "fizz" "seven hundred and sixty-one thousand five hundred and thirty-nine" "buzz" "fizz" "seven hundred and sixty-one thousand five hundred and forty-two" "seven hundred and sixty-one thousand five hundred and forty-three" "sheeshfizz" "buzz" "seven hundred and sixty-one thousand five hundred and forty-six" "fizz" "seven hundred and sixty-one thousand five hundred and forty-eight" "seven hundred and sixty-one thousand five hundred and forty-nine" "fizzbuzz" "sheesh" "seven hundred and sixty-one thousand five hundred and fifty-two" "fizz" "seven hundred and sixty-one thousand five hundred and fifty-four" "buzz" "fizz" "seven hundred and sixty-one thousand five hundred and fifty-seven" "sheesh" "fizz" "buzz" "seven hundred and sixty-one thousand five hundred and sixty-one" "fizz" "seven hundred and sixty-one thousand five hundred and sixty-three" "seven hundred and sixty-one thousand five hundred and sixty-four" "sheeshfizzbuzz" "seven hundred and sixty-one thousand five hundred and sixty-six" "seven hundred and sixty-one thousand five hundred and sixty-seven" "fizz" "seven hundred and sixty-one thousand five hundred and sixty-nine" "buzz" "fizz" "sheesh" "seven hundred and sixty-one thousand five hundred and seventy-three" "fizz" "buzz" "seven hundred and sixty-one thousand five hundred and seventy-six" "fizz" "seven hundred and sixty-one thousand five hundred and seventy-eight" "sheesh" "fizzbuzz" "seven hundred and sixty-one thousand five hundred and eighty-one" "seven hundred and sixty-one thousand five hundred and eighty-two" "fizz" "seven hundred and sixty-one thousand five hundred and eighty-four" "buzz" "sheeshfizz" "seven hundred and sixty-one thousand five hundred and eighty-seven" "seven hundred and sixty-one thousand five hundred and eighty-eight" "fizz" "buzz" "seven hundred and sixty-one thousand five hundred and ninety-one" "fizz" "sheesh" "seven hundred and sixty-one thousand five hundred and ninety-four" "fizzbuzz" "seven hundred and sixty-one thousand five hundred and ninety-six" "seven hundred and sixty-one thousand five hundred and ninety-seven" "fizz" "seven hundred and sixty-one thousand five hundred and ninety-nine" "sheeshbuzz" "fizz" "seven hundred and sixty-one thousand six hundred and two" "seven hundred and sixty-one thousand six hundred and three" "fizz" "buzz" "seven hundred and sixty-one thousand six hundred and six" "sheeshfizz" "seven hundred and sixty-one thousand six hundred and eight" "seven hundred and sixty-one thousand six hundred and nine" "fizzbuzz" "seven hundred and sixty-one thousand six hundred and eleven" "seven hundred and sixty-one thousand six hundred and twelve" "fizz" "sheesh" "buzz" "fizz" "seven hundred and sixty-one thousand six hundred and seventeen" "seven hundred and sixty-one thousand six hundred and eighteen" "fizz" "buzz" "sheesh" "fizz" "seven hundred and sixty-one thousand six hundred and twenty-three" "seven hundred and sixty-one thousand six hundred and twenty-four")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn word-up-to-one-million [n]
  (cond (< n 1000) (word-up-to-thousand n)
        (< n 1000000) (let [ y (quot n 1000) x (rem n 1000) ]
                        (if (= x 0)
                          (str (word-up-to-thousand y) " thousand")
                          (str (word-up-to-thousand y) " thousand" " " (word-up-to-thousand x))))))

(defn word-up-to-one-billion [n]
  (cond (< n 1000000) (word-up-to-one-million n)
        (< n 1000000000) (let [ y (quot n 1000000) x (rem n 1000000) ]
                           (if (= x 0)
                             (str (word-up-to-thousand y) " million")
                             (str (word-up-to-thousand y) " million" " " (word-up-to-one-million x))))))

;; My friend Mike was trying to tell me something incomprehensible about the gambler's fallacy.

;; In fact his bait was 'Did you know the gambler's fallacy actually turns out to be true?'

;; As I understand it, the gambler's fallacy is the common belief that
;; when tossing coins, rolling dice, or playing fruit machines,
;; a run of bad luck will make good luck more likely.

;; It seems to be related to the belief in a 'just world', where
;; people deserve what happens to them, and good and bad luck are
;; spread out evenly. Every cloud has a silver lining, and so on:

;; Mike said, first of all, that HHHH is as likely as HHHT.

;; This is clearly true if you toss four coins in order, but let's check:

(defn coin [] (rand-nth [:H :T])) ; #'user/coin

(coin) ; :T
(coin) ; :H

(defn four-tosses []
  [(coin)(coin)(coin)(coin)])

(four-tosses) ; [:T :H :T :H]

(def fours (repeatedly four-tosses))

fours ; ([:T :T :H :T] [:H :T :H :T] [:H :H :T :T] [:T :H :T :T] [:T :H :T :T] [:T :H :H :T] [:H :T :T :T] [:H :T :T :H] [:H :H :H :H] [:H :T :H :T] [:H :H :T :T] [:T :H :H :T] [:H :T :H :T] [:H :H :H :T] [:T :T :T :T] [:T :T :T :H] [:T :T :T :H] [:T :T :H :H] [:T :T :T :H] [:T :T :H :H] [:T :H :H :T] [:H :H :T :T] [:T :T :H :H] [:T :T :T :H] [:H :H :T :H] [:T :T :H :H] [:H :H :T :H] [:T :T :H :H] [:T :H :H :T] [:H :T :T :T] [:T :T :T :T] [:H :H :H :T] [:T :T :H :T] [:T :T :T :H] [:T :T :H :T] [:H :T :H :T] [:T :H :T :H] [:H :H :T :H] [:H :T :T :T] [:H :T :T :T] [:H :T :T :H] [:H :H :T :H] [:T :T :H :H] [:T :T :H :T] [:T :H :H :T] [:T :H :T :H] [:H :H :T :T] [:H :T :T :T] [:T :H :T :T] [:T :H :H :H] [:T :T :H :H] [:T :T :H :H] [:H :T :T :T] [:H :T :H :T] [:H :H :H :H] [:H :T :T :H] [:T :T :H :T] [:H :T :H :T] [:T :H :H :H] [:T :H :T :H] [:T :T :H :T] [:T :H :T :H] [:T :H :T :H] [:T :H :H :H] [:T :T :H :H] [:H :T :H :H] [:H :T :H :H] [:H :H :T :H] [:T :T :T :H] [:T :H :T :T] [:H :H :T :T] [:T :H :H :T] [:T :H :T :H] [:T :T :H :T] [:T :T :H :H] [:H :H :H :T] [:H :T :T :T] [:H :H :H :T] [:H :T :H :T] [:T :T :T :H] [:H :H :H :T] [:H :T :H :H] [:T :H :T :H] [:T :T :H :T] [:H :H :T :H] [:T :T :T :T] [:H :T :T :T] [:T :T :T :T] [:H :H :T :T] [:T :H :T :T] [:T :H :T :H] [:T :H :T :T] [:T :T :T :H] [:H :T :T :T] [:T :H :T :H] [:H :H :T :T] [:T :T :H :T] [:H :T :H :H] [:H :T :T :T] [:H :T :H :T] ...)

;; I don't like having to name something and then execute the name, so a quick macro to avoid having to do (def var 3) and then var immediately afterwards.
(defmacro define [a b] `(let [val# ~b] (def ~a val#) val#))

(define fours (repeatedly four-tosses)) ; ([:T :H :T :T] [:H :H :H :T] [:H :H :T :H] [:T :T :T :H] [:T :T :H :H] [:H :H :H :T] [:H :H :T :H] [:T :H :H :T] [:T :T :T :H] [:H :H :H :T] [:T :T :H :H] [:T :T :H :H] [:T :H :H :T] [:T :H :H :T] [:T :H :H :T] [:T :T :T :H] [:H :T :T :T] [:H :H :H :T] [:T :T :H :T] [:T :H :T :H] [:T :T :H :T] [:T :T :T :T] [:H :T :H :H] [:T :H :H :T] [:T :H :H :T] [:T :H :H :H] [:T :H :H :H] [:T :T :H :H] [:T :H :T :T] [:H :H :H :H] [:T :T :H :T] [:H :T :H :H] [:T :H :H :H] [:T :H :H :H] [:T :T :H :T] [:T :T :H :H] [:H :T :T :H] [:H :H :H :H] [:H :H :H :T] [:T :H :H :T] [:T :T :H :H] [:H :H :T :T] [:T :T :H :T] [:T :T :H :T] [:H :T :H :T] [:H :H :H :H] [:H :H :T :T] [:T :H :T :T] [:H :T :T :T] [:H :H :H :H] [:H :H :H :T] [:T :H :T :T] [:T :H :T :H] [:H :T :T :H] [:T :H :T :H] [:H :T :T :T] [:H :T :T :T] [:T :T :H :T] [:T :H :T :H] [:H :T :T :T] [:T :H :T :H] [:T :T :H :H] [:T :H :H :H] [:H :T :H :T] [:T :T :H :T] [:T :T :H :T] [:T :T :H :H] [:T :T :T :H] [:H :T :H :H] [:H :H :H :H] [:H :H :H :H] [:T :T :T :H] [:H :T :T :H] [:T :T :H :H] [:H :H :H :H] [:T :T :T :H] [:T :H :T :T] [:T :T :H :T] [:H :T :T :H] [:T :H :T :T] [:T :H :H :H] [:T :H :H :T] [:H :H :H :H] [:H :T :H :T] [:T :H :T :H] [:T :T :T :H] [:H :H :T :H] [:T :H :H :H] [:H :T :H :H] [:H :T :H :H] [:H :T :T :T] [:H :H :T :T] [:H :H :H :T] [:H :H :H :H] [:T :H :T :H] [:H :T :T :T] [:H :H :T :H] [:H :T :H :T] [:T :T :T :H] [:H :H :H :T] ...)



(def four-frequencies (frequencies (take 1000 fours)))

four-frequencies ; {[:H :T :H :T] 69, [:T :H :T :T] 70, [:H :H :H :H] 52, [:T :H :T :H] 68, [:H :H :T :H] 58, [:H :H :T :T] 67, [:T :T :H :T] 61, [:T :H :H :T] 61, [:H :T :T :T] 60, [:T :T :T :T] 60, [:H :T :H :H] 62, [:H :T :T :H] 70, [:T :T :T :H] 68, [:H :H :H :T] 50, [:T :H :H :H] 47, [:T :T :H :H] 77}

(map four-frequencies (list [:H :H :H :T] [:H :H :H :H])) ; (50 52)
;; That looks about right!

;; If we try it a million times, we see that it's as close as damn it
(map
 (frequencies (take 1000000 fours))
 [[:H :H :H :T]
  [:H :H :H :H]]) ; (62282 62369)

;; This is not a surprising result, in fact if it hadn't looked true my first thought would be a bug, and my second that the random number generator wasn't very random.


;; But then Mike said: and that's just as true if you take a long sequence of coin tosses and choose four consecutive ones:

(def coins (take 10000 (repeatedly coin))) 

(def setsoffour (partition 4 1 coins))

(def freqs (frequencies setsoffour))

(freqs [:H :H :H :H]) ; 618
(freqs [:H :H :H :T]) ; 618

;; Indeed, as Mike pointed out, if that wasn't true then if you saw three heads, you'd know something about what was coming up
;; next, which is the gambler's fallacy belief

;; And yet at this point I have a strong intuition that runs of three heads are less likely than runs of four heads.

;; A man who trusts his intuition on a question of probability is an idiot. Let's look:

(def runs (partition-by #(= % :T) coins))

runs ; ((:H) (:T) (:H :H :H) (:T :T) (:H :H :H :H :H :H) (:T) (:H :H) (:T) (:H) (:T) (:H :H) (:T :T) (:H) (:T :T :T :T) (:H :H) (:T :T) (:H :H :H) (:T) (:H :H) (:T :T :T :T) (:H) (:T :T :T :T) (:H :H) (:T :T) (:H) (:T) (:H) (:T :T) (:H) (:T) (:H) (:T) (:H) (:T :T :T) (:H) (:T :T :T :T :T :T) (:H :H) (:T) (:H) (:T :T) (:H) (:T :T) (:H) (:T) (:H) (:T :T :T :T :T :T :T :T :T :T :T) (:H) (:T :T :T :T) (:H :H) (:T) (:H :H :H :H :H :H) (:T) (:H) (:T) (:H :H) (:T) (:H :H) (:T :T :T) (:H :H :H) (:T) (:H) (:T) (:H :H :H :H) (:T :T :T :T :T :T) (:H :H :H) (:T) (:H :H) (:T) (:H :H) (:T :T :T :T) (:H :H) (:T :T) (:H) (:T :T) (:H :H :H :H) (:T) (:H) (:T) (:H) (:T :T :T :T) (:H :H :H) (:T) (:H) (:T) (:H) (:T) (:H) (:T :T :T :T) (:H) (:T :T) (:H) (:T) (:H :H) (:T :T) (:H) (:T) (:H) (:T :T) (:H :H :H :H) (:T :T) ...)

(let [a 5] (def var a) a)




(defn busfn[x]
  (cond (= x '(:H :H :H :H)) :red
        (= x '(:H :H :H :T)) :green
        :else '-))

(def buses (map busfn setsoffour))

coins ; (:H :H :T :T :T :H :T :T :T :H :H :T :H :H :H :T :H :T :T :H :T :T :H :T :T :H :H :T :T :H :T :T :T :H :T :H :H :H :T :H :T :T :T :H :H :H :T :H :H :T :H :T :T :H :T :H :T :T :H :T :H :T :T :T :T :H :T :T :T :T :T :T :H :T :H :H :H :T :H :H :T :H :H :T :H :T :T :H :T :H :T :H :T :T :H :H :H :T :H :T ...)
(drop 100 coins) ; (:T :H :H :H :T :T :T :T :H :H :H :H :H :H :T :H :T :T :T :H :T :H :T :T :T :H :T :H :H :H :T :T :T :H :T :H :T :T :T :T :T :T :T :T :H :H :H :T :H :H :T :T :H :T :H :T :T :H :H :T :H :T :H :T :T :H :T :H :T :T :T :H :T :H :H :H :H :H :H :T :H :T :H :H :H :T :T :T :T :H :T :T :H :H :H :T :T :T :T :H ...)
(filter identity buses) ; (- - - - - - - - - - - - :green - - - - - - - - - - - - - - - - - - - - - - :green - - - - - - - :green - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - :green - - - - - - - - - - - - - - - - - - - :green - - - - - ...)
(drop 100 (filter identity buses)) ; (- :green - - - - - - :red :red :red :green - - - - - - - - - - - - - - - :green - - - - - - - - - - - - - - - - :green - - - - - - - - - - - - - - - - - - - - - - - - - - - - :red :red :red :green - - - - - :green - - - - - - - - - :green - - - - - - - ...)
(drop 200 (filter identity buses)) ; (- - - - - - - - - - - - - - - :red :green - - - - - - - - - :green - - - - - - - - - - - :green - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - :red :red :green - - - - - - - - - - - - - - - - - - - - ...)

(def splits (partition-by #(= % '-)  buses))

(def timetable (map (fn[x] (if (= (first x) '-) (count x) x)) splits))

(sort-by second (frequencies timetable)) ;
([64 1]
 [47 1]
 [(:red :red :red :red :red :red :red :red :red :red :red :green) 1]
 [50 1]
 [(:red :red :red :red :red :red :red :red :red :red :green) 1]
 [83 1]
 [54 1]
 [56 1]
 [57 1]
 [33 2]
 [34 2]
 [40 2]
 [42 2]
 [55 2]
 [58 2]
 [60 2]
 [35 3]
 [36 3]
 [43 3]
 [46 3]
 [(:red :red :red :red :red :red :red :green) 3]
 [49 3]
 [29 3]
 [32 4]
 [37 4]
 [38 4]
 [41 4]
 [39 5]
 [23 5]
 [27 5]
 [30 6]
 [31 6]
 [22 8]
 [24 8]
 [28 8]
 [18 9]
 [21 9]
 [25 9]
 [26 9]
 [(:red :red :red :red :red :red :green) 10]
 [20 10]
 [19 11]
 [15 12]
 [(:red :red :red :red :red :green) 12]
 [11 17]
 [16 17]
 [13 18]
 [14 18]
 [17 23]
 [(:red :red :red :red :green) 26]
 [8 31]
 [10 31]
 [12 31]
 [6 34]
 [9 34]
 [4 35]
 [5 39]
 [7 39]
 [(:red :red :red :green) 42]
 [3 60]
 [(:red :red :green) 71]
 [(:red :green) 135]
 [(:green) 300])


;; Can look at this as a Markov Chain and ask about the waiting times

(use 'clojure.core.matrix)

(def A [[1 0 0 0 1 0 0 0]
        [1 0 0 0 1 0 0 0]
        [0 1 0 0 0 1 0 0]
        [0 1 0 0 0 1 0 0]
        [0 0 1 0 0 0 1 0]
        [0 0 1 0 0 0 1 0]
        [0 0 0 1 0 0 0 1]
        [0 0 0 1 0 0 0 1]])

(clojure.core.matrix/mmul A A)

(clojure.core.matrix/mmul A A A)

(use 'clojure.core.matrix.linear)

(set-current-implementation :clatrix)

(clojure.core.matrix.linear/svd A)

(pprint (clojure.core.matrix.linear/eigen A))



(def run '(- :green - - - - - - :red :red :red :green - - - - - - - - - - - - - - - :green))
(1 :green 9 8 7 6 5 4 3    2    1    :green 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 :green)

(+
 (reduce + (range 0 2))
 (reduce + (range 0 10)) 
 (reduce + (range 0 16))) ; 166

(* 1/2 (+ 1 15) 15)

(def green-waits (map first (partition 2 (map count (partition-by #(= % :green)  run)))))

green-waits ; (1 9 15)

(map (fn[x] (reduce + (range 0 (inc x)))) green-waits) ; (1 45 120)

(reduce + (map (fn[x] (reduce + (range 0 (inc x)))) green-waits))

(defn running-average [sq]
  (map #(apply / %)
       (drop 1
             (reductions
              (fn [[sum count] x] [(+ sum x) (inc count)])
              [0 0]
              sq))))

(running-average '(1. 0 9 8 7 6 5 4 3 2 1 0 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0))

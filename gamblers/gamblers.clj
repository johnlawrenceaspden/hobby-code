;; The Gambler's Fallacy

;; Runs of good and bad luck

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A quick prelude where I change some things about the language to save typing later on.
;; Ignore this bit, or see: http://www.learningclojure.com/2015/12/define-macro.html for the gories.

;; I don't like having to name something and then execute the name to find out what it is, so a
;; quick macro to avoid having to do (def var 3) and then var immediately afterwards.
(defmacro define [a b] `(let [val# ~b] (def ~a val#) val#))

;; And I want to talk about infinite sequences, so as a favour to my readers, I set
(set! *print-length* 100)
;; which will stop your repls turning to stone should you inadvertently look at the medusa
;; I have this set all the time!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My friend Mike was trying to tell me something incomprehensible about the gambler's fallacy.

;; In fact his bait was 'Did you know the gambler's fallacy actually turns out to be true?'

;; As I understand it, the gambler's fallacy is the common belief that when tossing coins, rolling
;; dice, or playing fruit machines, a run of bad luck will make good luck more likely.

;; It seems to be related to the belief in a 'just world', where people deserve what happens to
;; them, and good and bad luck are spread out evenly. Every cloud has a silver lining, and so on:

;; Mike said, first of all, that HHHH is as likely as HHHT.

;; This is clearly true if you toss four coins in order, but let's check:

(defn coin [] (rand-nth [:H :T])) ; #'user/coin

(coin) ; :T
(coin) ; :H

(defn four-tosses []
  [(coin)(coin)(coin)(coin)])

(four-tosses) ; [:T :H :T :H]

(define fours (repeatedly four-tosses)) ; ([:T :T :H :H] [:T :H :H :T] [:T :T :T :H] [:H :T :H :H] [:T :T :T :T] [:H :T :H :H] [:H :T :T :T] [:H :H :T :T] [:H :H :H :T] [:H :H :H :H] [:T :T :T :T] [:T :T :T :T] [:T :H :H :T] [:H :H :T :H] [:H :H :T :T] [:T :T :H :T] [:H :H :H :T] [:T :H :T :T] [:H :H :H :H] [:H :H :H :T] [:H :H :H :H] [:H :H :T :T] [:H :H :H :H] [:H :H :H :H] [:H :T :T :T] [:T :T :H :T] [:H :T :T :H] [:H :T :T :H] [:H :T :H :H] [:H :T :H :H] [:H :T :H :T] [:T :H :T :H] [:H :T :T :T] [:H :H :T :T] [:H :T :T :T] [:T :H :H :H] [:T :T :T :T] [:H :T :H :H] [:H :H :H :T] [:T :T :H :T] [:T :H :T :T] [:T :T :H :T] [:H :T :T :T] [:T :H :T :H] [:H :H :T :H] [:H :T :T :T] [:T :H :T :T] [:H :H :T :H] [:T :H :H :T] [:T :H :T :T] [:H :H :T :H] [:H :H :H :T] [:T :H :H :H] [:T :H :T :H] [:T :T :T :H] [:T :H :T :H] [:H :T :T :H] [:T :H :H :H] [:H :H :H :T] [:T :H :T :T] [:T :T :H :H] [:T :H :H :T] [:H :T :T :T] [:H :H :H :H] [:H :H :T :H] [:H :T :H :H] [:T :T :T :H] [:T :T :T :T] [:T :T :H :H] [:T :T :T :H] [:T :T :H :H] [:T :H :H :H] [:T :H :T :T] [:T :T :T :H] [:T :T :H :H] [:T :H :T :H] [:T :T :H :H] [:H :H :H :T] [:H :T :T :H] [:H :H :H :T] [:T :H :H :T] [:H :H :T :T] [:H :H :T :H] [:H :H :T :T] [:T :T :T :H] [:H :H :H :T] [:T :H :T :T] [:H :H :T :H] [:H :H :T :T] [:H :H :T :H] [:H :H :T :H] [:H :T :T :H] [:H :T :T :T] [:T :T :H :H] [:T :H :T :T] [:T :H :T :T] [:T :H :T :T] [:H :H :H :T] [:H :T :T :T] [:T :T :H :H] ...)

(define four-frequencies (frequencies (take 1000 fours))) ; {[:H :T :H :T] 57, [:T :H :T :T] 65, [:H :H :H :H] 57, [:T :H :T :H] 62, [:H :H :T :H] 74, [:H :H :T :T] 62, [:T :T :H :T] 63, [:T :H :H :T] 65, [:H :T :T :T] 69, [:T :T :T :T] 64, [:H :T :H :H] 65, [:H :T :T :H] 59, [:T :T :T :H] 67, [:H :H :H :T] 69, [:T :H :H :H] 51, [:T :T :H :H] 51}

(map four-frequencies (list [:H :H :H :T] [:H :H :H :H])) ; (69 57)
;; That looks about right!

;; If we try it a million times, we see that it's as close as damn it
(map
 (frequencies (take 1000000 fours))
 [[:H :H :H :T]
  [:H :H :H :H]]) ; (62550 62352)

;; This is not a surprising result, in fact if it hadn't looked true my first thought would be a
;; bug, and my second that the random number generator wasn't very random.


;; But then Mike said: and that's just as true if you take a long sequence of coin tosses and choose
;; four consecutive ones:

(def coins (repeatedly coin))

(def setsoffour (partition 4 1 coins))

(def freqs (frequencies (take 10000 setsoffour)))

(freqs [:H :H :H :H]) ; 653
(freqs [:H :H :H :T]) ; 635

;; Indeed, as Mike pointed out, if that wasn't true then if you saw three heads, you'd know
;; something about what was coming up next, which is the gambler's fallacy belief.

;; And yet at this point I have a strong intuition that runs of three heads are less likely than
;; runs of four heads.

;; A man who trusts his intuition on a question of probability is an idiot. Let's look:

(define runs (partition-by #(= % :T) coins)) ; ((:T :T) (:H) (:T :T) (:H) (:T :T :T) (:H) (:T :T :T) (:H :H :H) (:T :T :T) (:H) (:T :T :T :T :T :T) (:H) (:T) (:H :H) (:T) (:H) (:T :T :T) (:H :H :H :H :H :H :H :H) (:T :T :T :T) (:H) (:T :T) (:H :H :H) (:T :T :T :T :T :T) (:H :H :H :H :H) (:T) (:H :H :H) (:T) (:H :H) (:T) (:H :H) (:T :T) (:H :H :H) (:T) (:H) (:T) (:H) (:T :T :T :T) (:H :H) (:T :T :T :T) (:H :H) (:T) (:H :H :H) (:T :T) (:H :H :H :H :H :H) (:T :T :T :T :T) (:H :H) (:T :T :T :T :T :T) (:H :H) (:T :T :T :T :T :T :T :T :T) (:H :H :H) (:T) (:H) (:T :T) (:H :H :H) (:T) (:H) (:T :T) (:H :H :H) (:T) (:H :H :H :H) (:T) (:H) (:T :T :T) (:H) (:T) (:H) (:T :T) (:H :H :H) (:T) (:H :H :H) (:T) (:H :H) (:T) (:H) (:T) (:H) (:T :T :T) (:H :H) (:T) (:H) (:T) (:H :H :H) (:T) (:H) (:T) (:H) (:T) (:H) (:T :T :T :T) (:H :H :H :H :H :H :H) (:T :T :T :T :T) (:H) (:T) (:H :H) (:T :T :T) (:H) (:T) (:H) (:T :T :T :T :T) (:H :H :H :H) ...)

(map count runs) ; (2 1 2 1 3 1 3 3 3 1 6 1 1 2 1 1 3 8 4 1 2 3 6 5 1 3 1 2 1 2 2 3 1 1 1 1 4 2 4 2 1 3 2 6 5 2 6 2 9 3 1 1 2 3 1 1 2 3 1 4 1 1 3 1 1 1 2 3 1 3 1 2 1 1 1 1 3 2 1 1 1 3 1 1 1 1 1 1 4 7 5 1 1 2 3 1 1 1 5 4 ...)

(sort-by val (frequencies (take 1000 runs))) ;
'([(:H :H :H :H :H :H :H :H :H) 1]
  [(:H :H :H :H :H :H :H :H :H :H :H :H :H :H :H) 1]
  [(:T :T :T :T :T :T :T) 1]
  [(:H :H :H :H :H :H :H :H :H :H :H) 1]
  [(:H :H :H :H :H :H :H :H :H :H) 1]
  [(:T :T :T :T :T :T :T :T) 2]
  [(:T :T :T :T :T :T :T :T :T) 2]
  [(:H :H :H :H :H :H :H) 3]
  [(:H :H :H :H :H :H :H :H) 4]
  [(:H :H :H :H :H :H) 8]
  [(:T :T :T :T :T :T) 9]
  [(:H :H :H :H :H) 15]
  [(:T :T :T :T :T) 16]
  [(:H :H :H :H) 29]
  [(:T :T :T :T) 40]
  [(:T :T :T) 70]
  [(:H :H :H) 71]
  [(:T :T) 115]
  [(:H :H) 125]
  [(:H) 241]
  [(:T) 245])

;; So it looks like runs of n are about half as common as runs of n-1, which is also what I'd have expected.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Anyway, Mike spoke of buses, where a sequence of three heads and a head is a red bus, and a
;; sequence of three heads and a tail is a green bus. Any other combination of four things is not a bus.

;; let's start again from the top

(defn coin [] (rand-nth [:H :T]))

(define coins (repeatedly coin))

(defn busfn[x]
  (cond (= x '(:H :H :H :H)) 'R
        (= x '(:H :H :H :T)) 'G
        :else '-))

(define buses (map busfn (partition 4 1 coins))) ; (- - - - - - - - - - - - - - R G - - - - - G - - - - - G - - - - - - G - - - - - - - - - - - - - - R R R G - - - R G - - - - - - - - - - G - - - - - - - - - - - - - - R G - - - R R G - - - - - - - - - ...)

;; Let's check that red buses come as often as green buses

(frequencies (take 1000000 buses)) ; {- 874598, R 62999, G 62403}

;; Red buses come 1/16th of the time, Green ones 1/16th of the time, 14/16ths of the time you smoke a cigarette to encourage them.

(define runs (partition-by #(= % '-)  buses)) ; ((- - - - - - - - - - - - - -) (R G) (- - - - -) (G) (- - - - -) (G) (- - - - - -) (G) (- - - - - - - - - - - - - -) (R R R G) (- - -) (R G) (- - - - - - - - - -) (G) (- - - - - - - - - - - - - -) (R G) (- - -) (R R G) (- - - - - - - - - - - - - - - - - - - - - - - - - -) (R G) (- - - - - - - -) (G) (- - - - - - - - - - - - - - - - - - - - - -) (G) (- - - - - - - - - - - - - - - -) (R R G) (- - - - - - - - - -) (G) (- - - - - - - -) (R R G) (- - - - - - - - - - - - - - - - - - -) (R G) (- - - - - -) (G) (- - - - -) (G) (- - - - - - - - - - - -) (G) (- - - - - - -) (R G) (- - - - -) (R G) (- - - - - - - - - - - -) (R G) (- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -) (G) (- - - - - - - - - - - - - - - - - -) (G) (- - - - - -) (R R R G) (- - - -) (G) (- - - - - - -) (R G) (- - - - - - - - - - - - - - - - - - - - - -) (G) (- - - - - - - - - - - - - - - - - - -) (G) (- - -) (R G) (- - - - -) (R G) (- - - - - - - - - - - - - - - -) (G) (- - -) (G) (- - - -) (R R G) (- - - - - -) (G) (- - - - -) (G) (- - - - - - - - - - - - - - - - -) (R G) (- - - - - - - - - - - - - - - - - - - - - - - -) (R R R R R G) (- - - - - - - -) (G) (- - - - - - - - - - - - - - -) (R G) (- - - - - -) (G) (- - - - - -) (R R R G) (- - - - - - - -) (G) (- - - - - - - -) (G) (- - - - -) (G) (- - - - - -) (R R R G) (- - - - - - - - - - - - - - - - -) (G) (- - - - - - - - - - - - - - - - - - - - - - -) (G) (- - - - - - - -) (G) (- - - - - - -) (G) ...)

(map first  (partition 2 runs))

;; Runs of buses always alternate with runs of cigarettes 
(define type1 (map first  (partition 2 runs))) ; ((- - - - - - - - - - - - - -) (- - - - -) (- - - - -) (- - - - - -) (- - - - - - - - - - - - - -) (- - -) (- - - - - - - - - -) (- - - - - - - - - - - - - -) (- - -) (- - - - - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - -) (- - - - - - - - - -) (- - - - - - - -) (- - - - - - - - - - - - - - - - - - -) (- - - - - -) (- - - - -) (- - - - - - - - - - - -) (- - - - - - -) (- - - - -) (- - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - -) (- - - - - -) (- - - -) (- - - - - - -) (- - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - -) (- - -) (- - - - -) (- - - - - - - - - - - - - - - -) (- - -) (- - - -) (- - - - - -) (- - - - -) (- - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - -) (- - - - - - - - - - - - - - -) (- - - - - -) (- - - - - -) (- - - - - - - -) (- - - - - - - -) (- - - - -) (- - - - - -) (- - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - -) (- - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - -) (- - -) (- - -) (- - - - - - - - - - -) (- - - - -) (- - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - -) (- - -) (- - - - - - - - - - - -) (- - - - - - - - - - - -) (- - - - - - - - - -) (- - - - - - - - - - -) (- - - -) (- - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - -) (- - - - - - - - -) (- - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - -) (- - - -) (- - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - - -) (- - - - - -) (- - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - -) (- - - - - - - - - - - - - - - - - - - -) (- - - - - -) (- - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - -) (- - - - -) (- - - - - - - - - - - - - - - - - - - - -) (- - - - - -) (- - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - -) (- - -) (- - - - -) (- - - - -) (- - - - - - -) (- - - - - - - - - - - - - - - - - - - -) (- - - - - - - - -) (- - - - - - -) (- - - -) (- - - - - -) (- - - - - - - - - - -) (- - - - - - - - -) (- - - - - -) ...)
(define type2 (map second (partition 2 runs))) ; ((R G) (G) (G) (G) (R R R G) (R G) (G) (R G) (R R G) (R G) (G) (G) (R R G) (G) (R R G) (R G) (G) (G) (G) (R G) (R G) (R G) (G) (G) (R R R G) (G) (R G) (G) (G) (R G) (R G) (G) (G) (R R G) (G) (G) (R G) (R R R R R G) (G) (R G) (G) (R R R G) (G) (G) (G) (R R R G) (G) (G) (G) (G) (G) (R R G) (G) (G) (R G) (R G) (G) (R R G) (R R G) (G) (R R R R R R R R R G) (R G) (G) (G) (G) (G) (R R R G) (R R G) (R G) (R R R G) (G) (G) (G) (G) (G) (R G) (G) (G) (R R R G) (G) (G) (G) (G) (G) (R R R R G) (G) (R G) (G) (R R G) (R G) (R G) (G) (G) (R R G) (R R R G) (G) (R R G) (G) (R G) (R R R G) ...)

;; but in the general case we don't know which comes first
(define cigaretteruns (if (= (ffirst type1) '-) type1 type2)) ; ((- - - - - - - - - - - - - -) (- - - - -) (- - - - -) (- - - - - -) (- - - - - - - - - - - - - -) (- - -) (- - - - - - - - - -) (- - - - - - - - - - - - - -) (- - -) (- - - - - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - -) (- - - - - - - - - -) (- - - - - - - -) (- - - - - - - - - - - - - - - - - - -) (- - - - - -) (- - - - -) (- - - - - - - - - - - -) (- - - - - - -) (- - - - -) (- - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - -) (- - - - - -) (- - - -) (- - - - - - -) (- - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - -) (- - -) (- - - - -) (- - - - - - - - - - - - - - - -) (- - -) (- - - -) (- - - - - -) (- - - - -) (- - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - -) (- - - - - - - - - - - - - - -) (- - - - - -) (- - - - - -) (- - - - - - - -) (- - - - - - - -) (- - - - -) (- - - - - -) (- - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - -) (- - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - -) (- - -) (- - -) (- - - - - - - - - - -) (- - - - -) (- - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - -) (- - -) (- - - - - - - - - - - -) (- - - - - - - - - - - -) (- - - - - - - - - -) (- - - - - - - - - - -) (- - - -) (- - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - -) (- - - - - - - - -) (- - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - -) (- - - -) (- - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - - -) (- - - - - -) (- - - - - - - - - - - -) (- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -) (- - - - - - - -) (- - - - - - - - - - - - - - - - - - - -) (- - - - - -) (- - - - - - - - - - - - - - - - - -) (- - - - - - - - - - - - - - - -) (- - - - -) (- - - - - - - - - - - - - - - - - - - - -) (- - - - - -) (- - - - - - - - - - - - - - - - - - - -) (- - - - - - - - - -) (- - -) (- - - - -) (- - - - -) (- - - - - - -) (- - - - - - - - - - - - - - - - - - - -) (- - - - - - - - -) (- - - - - - -) (- - - -) (- - - - - -) (- - - - - - - - - - -) (- - - - - - - - -) (- - - - - -) ...)
(define busruns       (if (= (ffirst type1) '-) type2 type1)) ; ((R G) (G) (G) (G) (R R R G) (R G) (G) (R G) (R R G) (R G) (G) (G) (R R G) (G) (R R G) (R G) (G) (G) (G) (R G) (R G) (R G) (G) (G) (R R R G) (G) (R G) (G) (G) (R G) (R G) (G) (G) (R R G) (G) (G) (R G) (R R R R R G) (G) (R G) (G) (R R R G) (G) (G) (G) (R R R G) (G) (G) (G) (G) (G) (R R G) (G) (G) (R G) (R G) (G) (R R G) (R R G) (G) (R R R R R R R R R G) (R G) (G) (G) (G) (G) (R R R G) (R R G) (R G) (R R R G) (G) (G) (G) (G) (G) (R G) (G) (G) (R R R G) (G) (G) (G) (G) (G) (R R R R G) (G) (R G) (G) (R R G) (R G) (R G) (G) (G) (R R G) (R R R G) (G) (R R G) (G) (R G) (R R R G) ...) ; 

;; But notice that red buses are always followed immediately by other buses, either green or red
;; Whereas green buses are never followed immediately by either type of bus.

busruns ; ((R G) (G) (G) (G) (R R R G) (R G) (G) (R G) (R R G) (R G) (G) (G) (R R G) (G) (R R G) (R G) (G) (G) (G) (R G) (R G) (R G) (G) (G) (R R R G) (G) (R G) (G) (G) (R G) (R G) (G) (G) (R R G) (G) (G) (R G) (R R R R R G) (G) (R G) (G) (R R R G) (G) (G) (G) (R R R G) (G) (G) (G) (G) (G) (R R G) (G) (G) (R G) (R G) (G) (R R G) (R R G) (G) (R R R R R R R R R G) (R G) (G) (G) (G) (G) (R R R G) (R R G) (R G) (R R R G) (G) (G) (G) (G) (G) (R G) (G) (G) (R R R G) (G) (G) (G) (G) (G) (R R R R G) (G) (R G) (G) (R R G) (R G) (R G) (G) (G) (R R G) (R R R G) (G) (R R G) (G) (R G) (R R R G) ...)

;; It looks as though you're going to be happier if your morning commute involves green buses, which come fairly well spread out
;; than if you need a red bus, where you wait ages and three come along at once.

;; We should look at the length of time you have to wait for the different types of bus:

(define redwaits (partition 2 (partition-by #(= % 'R) buses)))
(doseq [x (take 10 redwaits)] (println x))
;;((- - - - - - - - - - - - - - - - G - - - - - -) (R R)
;; (G - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -) (R R R R R R)
;; (G - - - - -) (R)
;; (G - - - - - - - - - - - - - - - - - - - - - - - - - - - G - - - - - -) (R)
;; (G - - -) (R) ...)

;; If you arrive at the stop, how long will you have to wait for a red bus?

(define redwaits1 (map (fn [[cigs buses]] (list (reverse (map inc (range (count cigs)))) (repeat (count buses) 0))) redwaits))
(doseq [x (take 10 redwaits1)] (println x))
;; ((23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) (0 0))
;; ((31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) (0 0 0 0 0 0))
;; ((6 5 4 3 2 1) (0))
;; ((35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) (0))
;; ((4 3 2 1) (0))
;; ((72 71 70 69 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) (0 0))
;; ((37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) (0 0 0 0 0))
;; ((39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) (0 0 0 0))
;; ((7 6 5 4 3 2 1) (0))
;; ((22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1) (0 0 0 0 0))

(define redwaits2 (apply concat (apply concat redwaits1))) ; (23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0 0 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0 0 0 0 0 0 6 5 4 3 2 1 0 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 ...)

(defn running-average [sq]
  (map #(apply / %)
       (drop 1
             (reductions
              (fn [[sum count] x] [(+ sum x) (inc count)])
              [0. 0]
              sq))))


(define avgredwaits (running-average redwaits2)) ; (23.0 22.5 22.0 21.5 21.0 20.5 20.0 19.5 19.0 18.5 18.0 17.5 17.0 16.5 16.0 15.5 15.0 14.5 14.0 13.5 13.0 12.5 12.0 11.5 11.04 11.807692307692308 12.481481481481481 13.071428571428571 13.586206896551724 14.033333333333333 14.419354838709678 14.75 15.030303030303031 15.264705882352942 15.457142857142857 15.61111111111111 15.72972972972973 15.81578947368421 15.871794871794872 15.9 15.902439024390244 15.880952380952381 15.837209302325581 15.772727272727273 15.688888888888888 15.58695652173913 15.46808510638298 15.333333333333334 15.183673469387756 15.02 14.843137254901961 14.653846153846153 14.452830188679245 14.24074074074074 14.018181818181818 13.785714285714286 13.543859649122806 13.310344827586206 13.084745762711865 12.866666666666667 12.655737704918034 12.451612903225806 12.34920634920635 12.234375 12.107692307692307 11.969696969696969 11.82089552238806 11.661764705882353 11.492753623188406 11.82857142857143 12.140845070422536 12.430555555555555 12.698630136986301 12.945945945945946 13.173333333333334 13.381578947368421 13.571428571428571 13.743589743589743 13.89873417721519 14.0375 14.160493827160494 14.268292682926829 14.36144578313253 14.44047619047619 14.505882352941176 14.55813953488372 14.597701149425287 14.625 14.640449438202246 14.644444444444444 14.637362637362637 14.619565217391305 14.591397849462366 14.553191489361701 14.505263157894737 14.447916666666666 14.381443298969073 14.306122448979592 14.222222222222221 14.13 ...)
;; Looks like you have to wait 14 steps for a red bus

(define avggreenwaits (->> buses
                           (partition-by #(= % 'G) )
                           (partition 2)
                           (map (fn [[cigs buses]] (list (reverse (map inc (range (count cigs)))) (repeat (count buses) 0))))
                           (apply concat)
                           (apply concat)
                           (running-average))) ; (16.0 15.5 15.0 14.5 14.0 13.5 13.0 12.5 12.0 11.5 11.0 10.5 10.0 9.5 9.0 8.5 8.0 8.0 7.947368421052632 7.85 7.714285714285714 7.545454545454546 7.3478260869565215 7.125 6.88 6.615384615384615 7.703703703703703 8.678571428571429 9.551724137931034 10.333333333333334 11.03225806451613 11.65625 12.212121212121213 12.705882352941176 13.142857142857142 13.527777777777779 13.864864864864865 14.157894736842104 14.41025641025641 14.625 14.804878048780488 14.952380952380953 15.069767441860465 15.159090909090908 15.222222222222221 15.26086956521739 15.27659574468085 15.270833333333334 15.244897959183673 15.2 15.137254901960784 15.057692307692308 14.962264150943396 14.851851851851851 14.727272727272727 14.589285714285714 14.43859649122807 14.275862068965518 14.101694915254237 13.916666666666666 13.721311475409836 13.516129032258064 13.301587301587302 13.1875 13.061538461538461 12.924242424242424 12.776119402985074 12.617647058823529 12.44927536231884 12.271428571428572 12.47887323943662 12.666666666666666 12.835616438356164 12.986486486486486 13.12 13.236842105263158 13.337662337662337 13.423076923076923 13.49367088607595 13.55 13.592592592592593 13.621951219512194 13.63855421686747 13.642857142857142 13.635294117647058 13.616279069767442 13.586206896551724 13.545454545454545 13.49438202247191 13.433333333333334 13.362637362637363 13.282608695652174 13.193548387096774 13.095744680851064 12.989473684210527 12.875 12.75257731958763 12.622448979591837 12.565656565656566 12.5 ...)
;; But about 12.5 for a green one



(doseq [x (range 21)]
  (let [n (reduce * (repeat x 2))]
    (printf "%10d %.2f %.2f"
            n
            (first (drop n avgredwaits))
            (first (drop n avggreenwaits)))
    (println)))

   ;;       1 0.50 3.50
   ;;       2 0.33 3.00
   ;;       4 6.60 2.00
   ;;       8 16.78 15.11
   ;;      16 19.94 20.00
   ;;      32 15.85 16.85
   ;;      64 21.18 12.55
   ;;     128 26.80 10.53
   ;;     256 40.84 10.15
   ;;     512 27.13 10.12
   ;;    1024 25.16 9.89
   ;;    2048 23.33 10.61
   ;;    4096 24.64 11.34
   ;;    8192 24.02 11.02
   ;;   16384 23.71 11.46
   ;;   32768 24.36 11.64
   ;;   65536 24.67 11.83
   ;;  131072 25.08 11.74
   ;;  262144 25.52 11.79
   ;;  524288 26.06 11.91
   ;; 1048576 25.95 12.01


;; It doesn't look totally unreasonable to guess that the waiting time for green buses is 12 and
;; that for red buses is 26.

;; Which is to say that, if you're waiting for them, the red ones look much less frequent than the green
;; ones, even though come equally often.

;; So in a sense, HHHH is less likely than HHHT, which is kind of weird, if you think about it.





;; ;; Can look at this as a Markov Chain and ask about the waiting times

;; (use 'clojure.core.matrix)

;; (def A [[1 0 0 0 1 0 0 0]
;;         [1 0 0 0 1 0 0 0]
;;         [0 1 0 0 0 1 0 0]
;;         [0 1 0 0 0 1 0 0]
;;         [0 0 1 0 0 0 1 0]
;;         [0 0 1 0 0 0 1 0]
;;         [0 0 0 1 0 0 0 1]
;;         [0 0 0 1 0 0 0 1]])

;; (clojure.core.matrix/mmul A A)

;; (clojure.core.matrix/mmul A A A)

;; (use 'clojure.core.matrix.linear)

;; (set-current-implementation :clatrix)

;; (clojure.core.matrix.linear/svd A)

;; (pprint (clojure.core.matrix.linear/eigen A))


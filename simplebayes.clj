;; Bayes' Rule

;; Probability theory is like logic, but it works for more stuff. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Suppose we have a little electronic number generator. It has a button. 

;; When you press the button it displays a random result.

;; There are two types. 

;; Here is a factory for making such calculators:

(defn make-calculator []
  (if (< 0.5 (rand))
    (fn [] (inc (rand-int 6)))
    (fn [] (rand-nth ["heads" "tails"]))))

;; And here is a chap, who when given such a box, will press the button eight times, write the results down, and give them to us.

(defn chap [calculator]
    [(calculator)
     (calculator)
     (calculator)
     (calculator)
     (calculator)
     (calculator)
     (calculator)
     (calculator)]))

;; Let us try our scheme
(chap (make-calculator))    ;; -> [2 4 6 5 5 1 1 5]

;; In this case, our calculator produced a six and some fives, so we know that it's a dice-roller

;; Try again:
(chap (make-calculator))    ;; -> ["tails" "tails" "tails" "tails" "tails" "heads" "tails" "tails"]

;; This time we obviously got one that tosses coins.

;; We can do this using classical logic. Coins produce "heads" or "tails".
;; Therefore a thing which produces something that is not a head and not a tail is not a coin. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's a situation where that doesn't work so well:

;; We've got either six sided (D6) or four sided (D4) dice

(defn make-calculator-for-dandd []
  (if (< 0.5 (rand))
    (fn [] (inc (rand-int 6)))
    (fn [] (inc (rand-int 4)))))


(chap (make-calculator-for-dandd)) ;; -> [2 3 4 6 4 2 2 6]

;; Logic can help us here: Four sided dice don't make sixes, so a thing which makes a six can't be one. 

(chap (make-calculator-for-dandd)) ;; -> [2 1 4 3 2 3 1 1]

;; But what about now? Either type of dice can make 1,2,3 or 4. So logic can't tell us anything here. It could be a four sided dice or a six sided dice made that sequence.

;; Really? Then would you bet me £50 that that was a six sided one?

;; It turns out that the little calculators can be identified by the manufacturer:
(defn make-calculator-with-version []
  (if (< 0.5 (rand))
    (fn ([] (inc (rand-int 6)))([x](if (= x :ver) "six-sided")))
    (fn ([] (inc (rand-int 4)))([x](if (= x :ver) "four-sided")))))


(let [c (make-calculator-with-version)]
  [(c)(c)(c)(c)(c :ver)]) ;; -> [4 3 6 6 "six-sided"]

;; Let's roll a few:

(for [i (range 10)]
  (let [c (make-calculator-with-version)]
    [(c)(c)(c)(c)(c :ver)]))

;; ([4 4 4 3 "four-sided"]
;;  [2 3 1 1 "four-sided"]
;;  [3 5 1 2 "six-sided"]
;;  [1 4 3 2 "four-sided"]
;;  [4 1 2 1 "four-sided"]
;;  [1 4 2 1 "four-sided"]
;;  [1 1 4 2 "four-sided"]
;;  [1 1 4 1 "four-sided"]
;;  [1 1 1 1 "four-sided"]
;;  [4 6 5 1 "six-sided"])

;; Every time we've seen a 6 or a 5, we've been able to conclude that we had a six-sided dice.

;; Every time we haven't, it turns out that it was four sided.

;; If you'd been making bets on the basis that there was no way to know, you'd have lost heavily.










Now suppose there had been a factory, which once upon a time, had made a great number of two buttn rd number generators, but it had never been that careful about which button was which, and theink iusdon the buttons, although it was as stable as anything in tests, reacted badly with sweat in the fild and so nowadays there is no knowing which button is which.
(defmacro make-dodgy-calculator[& code]
 `(if (< 0.5 (rand))
   (fn [x#] (if (= x# :A) ~@code))
    (fn [x#] (if (= x# :B) ~@code))))

;; One of their most popular calculators was for players of the game "Dungeons and Dragons", and 
;; it could simulate the roll of either a four sided (D4), or a six sided dice (D6).

(defn make-dandd-calculator []
  (make-dodgy-calculator
           (inc (rand-int 6))
           (inc (rand-int 4))))

(chap make-dandd-calculator) ;; -> [0 1 5 5 1 3 3 2]

;; Again, it is pretty obvious what is going on here. button A is the one for the D6, and so button B must be for the D4.
;; Again, classical logic will do for us here.

;; If button A rolled a D4, then it would produce numbers 1,2,3,or 4. Since it has produced a 5, it does not roll a D4. Therefore it rolls a D6.

;; But sometimes, classical logic cannot help us:

(chap make-dandd-calculator) ;; -> [2 1 2 3 4 2 4 4]

;; Neither button has produced a 5, nor has either produced a 6. We have no contradiction, and so we have learned nothing.

;; What are the chances of that?

((frequencies
  (map (fn[x] (filter #{5 6} x))
      (map (fn[x](x))
           (repeat 10000 (fn[](chap make-dandd-calculator)))))) '()) ;; -> 1975

;; So about 1975 in every 10000. 

;; We can guess that a chap picking up a calculator, who presses each button 4 times, will find himself in a position to write on the buttons around 65 times in every 81 tries, and without any information at all the other 16.

;; And in fact, that is the best that can be done.

;; What about the more fiendish case of the 











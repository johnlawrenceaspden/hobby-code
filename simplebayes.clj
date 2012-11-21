;; Bayes' Rule

;; Probability theory is like logic, but it works for more stuff. 

;; Here are some really simple model problems to do with dice
;; rolls. Some we can solve by logic, with some, there is no definite
;; answer, but we can say interesting stuff about the probability of
;; various things

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Suppose we have a little electronic number generator. It has a button. 

;; When you press the button it displays a random result.

;; There are two types. Some simulate six sided dice: 

(defn d6 [] (inc (rand-int 6)))

;; Others simulate coins:

(defn coin [] (rand-nth ["heads" "tails"]))

;; Here is how we use them:

(d6)   ; -> 1
(coin) ; -> "heads"

;; Here is a factory for making such random number generators. It
;; makes about the same number of each:

(defn make-calculator []
  (rand-nth [ d6 coin ]))

;; And here is a little robot, who when given such a box, will press
;; the button eight times.

(defn robot [calculator]
  (into [] (for [i (range 8)] (calculator))))

;; Let us try our scheme
(robot (make-calculator))    ;; -> [2 4 6 5 5 1 1 5]

;; In this case, our calculator produced numbers, so we know that it's a dice-roller

;; Try again:
(robot (make-calculator))    ;; -> ["tails" "tails" "tails" "tails" "tails" "heads" "tails" "tails"]

;; This time we obviously got one that tosses coins.

;; We can do this using classical logic. Coins produce "heads" or
;; "tails".  Therefore a thing which produces something that is not a
;; head and not a tail is not a coin.

;; So we can make an assessor robot that will always be right, as long
;; as we give it one of these two types. Since he is an artificial
;; intelligence based on classical logic, we shall call him aristotle

(defn aristotle [calculator]
  (let [a (calculator)]
    (if (#{"heads" "tails"} a) "I am totally confident that this is a coin-tosser"
        "I am totally confident that this is a six-sided dice-roller")))


;; Now, by the magic of classical logic, the assessor robot can,
;; whilst only pressing the button once, decide which type of
;; calculator we have.  We'll then pass that calculator to the usual
;; robot to do a few test presses, so we can see whether the assessor
;; was right or not.
(let [c (make-calculator)]
  (list (aristotle c)
        (robot c)))

;; Let's also make an exhaustive test robot, which will press the button 10000 times, and record the results. We don't want to use this robot too often, because it takes ages to do its thing.

(defn assayer-robot [calculator]
  (frequencies (for [i (range 10000)] (calculator))))

;; So, we have made some little random number generating machines, a robot which can get them to generate random numbers, an assayer which, by doing lots and lots of trials, can empirically measure the odds of each outcome, and a wise robot which, by looking at the result of only one trial, can come up with a precise judgement about the type of random number generator that it's been given.

;; Let's get a calculator from the factory, and pass it to each robot in turn
(let [c (make-calculator)]
  (list (aristotle c)
        (robot c)
        (assayer-robot c))) ;; ->

;; ("I am totally confident that this is a coin-tosser"
;; ["tails" "tails" "tails" "heads" "tails" "heads" "tails" "tails"]
;; {"tails" 4919, "heads" 5081})

;; Notice that the assayer robot is much more robust than the assessor robot.

;; In order to DO INFERENCE, the assessor robot has had to MAKE
;; ASSUMPTIONS. Namely that the only two possibilities are d6s and
;; coins.

;; Of course, as every schoolboy knows, some dice have four sides:

(defn d4 [] (inc (rand-int 4)))

(robot d4)          ;; -> [2 4 2 4 2 2 4 2]
(assayer-robot d4)  ;; -> {3 2557, 1 2565, 2 2445, 4 2433}
(aristotle d4) ;; -> "I am totally confident that this is a six-sided dice-roller"

;; If the assumptions are violated, the assessor robot can be wrong.

;; But as long as its assumptions are true, its deductions will be correct.

;; Such is the mighty power of classical logic.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's a situation where classical logic doesn't work quite so well:

;; As well as coins, we've got both sorts of dice

(defn make-any-calculator []
  (rand-nth [coin d6 d4]))

;; Our simple minded robots still work well
(let [c (make-any-calculator)]
  [(robot c) (assayer-robot c) (aristotle c)])
;; -> [[3 1 3 1 3 4 3 3] {2 2527, 4 2430, 1 2514, 3 2529} "I am totally confident that this is a six-sided dice-roller"]

;; But the conclusions of the assessor aren't so good. 

;; Logic can help us here: Four sided dice don't make sixes, so a thing which makes a six can't be one. 

;; Let's build a new AI incorporating this knowledge

(defn alexander [calculator]
  (let [a (calculator)]
    (cond (#{"heads" "tails"} a) "I am totally confident that this is a coin-tosser"
          (#{5 6} a) "I am totally confident that this is a six-sided dice-roller"
          :else "All we can know is that it's a dice-roller of some kind")))

(let [c (make-any-calculator)]
  [(robot c) (assayer-robot c) (alexander c)]) ;;-> [[6 6 4 3 5 4 4 2] {1 1630, 3 1689, 5 1690, 6 1674, 4 1667, 2 1650} "All we can know is that it's a dice-roller of some kind"]

;; Hmm. I reckon it might be a bit premature here.
(defn alexander-two [calculator]
  (let [a (calculator)]
    (cond (#{"heads" "tails"} a) "I am totally confident that this is a coin-tosser"
          (#{5 6} a) "I am totally confident that this is a six-sided dice-roller"
          :else (alexander calculator))))

(let [c (make-any-calculator)]
  [(robot c) (assayer-robot c) (alexander-two c)]) ;;-> 5[[1 2 1 6 2 6 3 1] {3 1668, 1 1684, 5 1656, 4 1649, 2 1657, 6 1686} "I am totally confident that this is a six-sided dice-roller"]











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

;; In fact, if you've seen four rolls and you haven't seen a five or a six, you can be reasonably confident that you've got a D4 rather than a D6.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; How confident exactly?

;; Suppose you had 120 of these little random number generators.

;; Sixty are D4s, and sixty are D6s. And you press the buttons on each one. By an amazing bit of luck, the distributions of the answers are 'just right'.

;; You'd have 10 of the D6s displaying 6, and 10 of them displaying 5. These ones, we're absolutely sure about. They have to be D6s.

;; All the others would be showing 1,2,3 or 4.

;; So we've got 100 that can be D4 and D6, but we know that 60 of them are D4s, and only 40 of them are D6s.

;; In other words, we started off with odds of 60:60. We've made one observation of each generator, and now we've split them into two groups.

;; The ones that showed 5 and 6 we're now certain are D6s. The other ones form a group where the odds are 60:40 in favour of D4.
























;;Now suppose there had been a factory, which once upon a time, had made a great number of two buttn rd number generators, but it had never been that careful about which button was which, and theink iusdon the buttons, although it was as stable as anything in tests, reacted badly with sweat in the fild and so nowadays there is no knowing which button is which.
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











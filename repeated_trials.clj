;; If I have a 1000000 sided die (numbered 0 to 999999), and I roll it 1000000 times,
;; how many times will I roll a 0?

;; Let us consider a simpler case. I have a 2 sided die, I roll it twice, how many times will I get a 0?

;; Possible results 00 01 10 11, so there is:
;; 1/4 chance of 0 zeroes, 1/4+1/4 chance of 1 zero, 1/4 chance of 2 zeroes.

;; What about a 3 sided die?

;; Possible results
;; Here is our space of all outcomes, Omega. The probability of every event in Omega is 1/27, and there are 27 events
;; 000 001 002 010 011 012 020 021 022  100 101 102 110 111 112 120 121 122  200 201 202 210 211 212 220 221 222
;; Here are the corresponding values of X, the number of zeroes, for each event in Omega
;; 3   2   2   2   1   1   2   1   1    2   1    1   1   0   0   1   0  0    2   1   1   1    0   0   1   0   0

;; There are 8 zeroes, 12 ones, 6 twos, and 1 three, and that adds up to 27, which is good.

;; 8/27 is the probability of 0 zeroes, 12/27 is the probability of 1 zero, 6/27 is the probability of 2 zeroes, 1/27 is the probability of 3 zeroes.

;; What about a 4 sided die?
;; Enough of this silliness. Computers should do simple repetitive tasks.

(defn cartesian-product [lista listb]
  (for [a lista b listb] (concat a b)))

(defn power [n m]
  (if (zero? m) 1
      (* n (power n (dec m)))))

(defn omega [sides trials]
  (cond (= trials 0) '()
        (= trials 1) (map list (range sides))
        :else (cartesian-product (omega sides 1) (omega sides (dec trials)))))

(defn zeroes [list]
  (count (filter zero? list)))

(defn no-of-zeroes [n]
  (into (sorted-map) (for [[k v] (frequencies (map zeroes (omega n n)))] [k (/ v (power n n))])))

;; here are our previous results
(no-of-zeroes 2) ;; {0 1/4, 1 1/2, 2 1/4}
(no-of-zeroes 3) ;; {0 8/27, 1 4/9, 2 2/9, 3 1/27}

;; And here are some more
(no-of-zeroes 4);; {0 81/256, 1 27/64, 2 27/128, 3 3/64, 4 1/256}

;; Let's look at the probabilities as decimals
(map double (vals (no-of-zeroes 4))) ;;(0.31640625 0.421875 0.2109375 0.046875 0.00390625)

;; What does the sequence look like?
(map #(map double (vals (no-of-zeroes %))) (range 7))

#_(()
 (1.0)
 (0.25               0.5                0.25)
 (0.2962962962962963 0.4444444444444444 0.2222222222222222 0.03703703703703704)
 (0.31640625         0.421875           0.2109375          0.046875             0.00390625)
 (0.32768            0.4096             0.2048             0.0512               0.0064               3.2E-4)
 (0.3348979766803841 0.4018775720164609 0.2009387860082305 0.05358367626886145  0.008037551440329218 6.430041152263374E-4 2.143347050754458E-5))

;; It looks as though these numbers might be settling down to something. But to go much further, we'll need a quicker way of working out these probabilities than brute force counting.

;; Here are all the possible values of three rolls of a three sided die.
;; 000 001 002 010 011 012 020 021 022  100 101 102 110 111 112 120 121 122  200 201 202 210 211 212 220 221 222
;; All the different outcomes here are equally likely, the chance of each one is 1/(3x3x3)
;; We get three zeroes only from 000. There's only one way this can happen.
;; We get two zeros from 001, 002, 010, 020, 100, 200. There are three different places that can be non-zero, and each place can be either 1 or 2, so the number of different ways in which two zeroes can happen is 2x3 = 6.
;; We can get one zero from 0xx, x0x, and xx0, and for each of these three cases, each x can be either 1 or 2, so there are 3x4=12 cases.
;; No zeroes has to happen in the pattern xxx, where each x is 1 or 2, so 2x2x2=8
;; From these we get the 1/27, 6/27, 12/27, and 8/27, that our brute-force calculation gave.

;; In the more general case, if we have n sided dice, and want to know the probability of rolling i zeroes, then
;; it's (n-1)^(n-i) times the number of ways of making a set of i from n things.

(defn factorial [n]
  (if (zero? n) 1 (* n (factorial (dec n)))))

(defn choose [n i]
  (/ (factorial n) (factorial i) (factorial (- n i))))

(choose 3 3)

(defn probability-of-i-zeroes-on-n-n-sided-dice [n i]
  (/ (* (power (dec n) (- n i)) (choose n i)) (power n n)))

(defn no-of-zeroes [n]
  (map #(probability-of-i-zeroes-on-n-n-sided-dice n %) (range (inc n))))

(map #(map double (no-of-zeroes %)) (range 10))


;; Here's our sequence of sequences, which has an air of converging to something
#_((1.0)
   (0.0 1.0)
   (0.25 0.5 0.25)
   (0.2962962962962963 0.4444444444444444 0.2222222222222222 0.03703703703703704)
   (0.31640625 0.421875 0.2109375 0.046875 0.00390625)
   (0.32768 0.4096 0.2048 0.0512 0.0064 3.2E-4)
   (0.3348979766803841 0.4018775720164609 0.2009387860082305 0.05358367626886145 0.008037551440329218 6.430041152263374E-4 2.143347050754458E-5)
   (0.3399166770891137 0.396569456603966 0.198284728301983 0.05507909119499528 0.009179848532499214 9.179848532499214E-4 5.099915851388452E-5 1.214265678902012E-6)
   (0.3436089158058167 0.3926959037780762 0.1963479518890381 0.05609941482543945 0.0100177526473999 0.001144886016845703 8.177757263183594E-5 3.337860107421875E-6 5.960464477539062E-8)
   (0.3464394161146186 0.3897443431289459 0.1948721715644729 0.05683771670630461 0.01065707188243211 0.001332133985304014 1.110111654420012E-4 5.947026720107206E-6 1.858445850033502E-7 2.581174791713197E-9)
   )

;; Let's look a bit further down
(take 10 (map double (no-of-zeroes 100)))
;; (0.3660323412732295 0.3697296376497268 0.1848648188248634 0.06099916580753068 0.01494171485689514 0.002897787123761482 4.634508026217858E-4 6.286345663268091E-5 7.381693771261773E-6 7.621950919821359E-7)

;; These look awfully like 1/e*(1 1 1/2! 1/3! 1/4! 1/5! ....)
(map #(/ 1 (Math/exp 1) (factorial %)) (range 10))
;;(0.3678794411714423 0.3678794411714423 0.18393972058572114 0.06131324019524038 0.015328310048810094 0.003065662009762019 5.109436682936698E-4 7.29919526133814E-5 9.123994076672675E-6 1.0137771196302974E-6)

(take 10 (map double (no-of-zeroes 1000)))
;;(0.367695424770964 0.3680634882592233 0.1840317441296116 0.06128250938984064 0.01528995542083862 0.003048807927758812 5.061000814347711E-4 7.193814971345093E-5 8.938261094276373E-6 9.86181181795369E-7)


;; What can we do with this?

;; Well, say that we want to check whether an event has a one in one thousand chance of happening?

;; If you look at one thousand cases where the event had a chance of happening, then you'd expect no cases 36% of the time, one case 36% of the time, two cases 18% of the time, and three cases 6% of the time. But any higher number of cases would seem unlikely. In fact any numbers other than zero, one, or two should be suspicious.








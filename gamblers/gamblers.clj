;; The Gambler's Fallacy

;; Runs of good and bad luck

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A quick prelude where I change some things about the language to save typing later on.
;; Ignore this bit, or see: http://www.learningclojure.com/2015/12/define-macro.html for the gories.

;; I don't like having to name something and then execute the name, so a quick macro to avoid having
;; to do (def var 3) and then var immediately afterwards.
(defmacro define [a b] `(let [val# ~b] (def ~a val#) val#))

;; And I want to talk about infinite sequences, so as a favour to my readers, I set
(set! *print-length* 100)
;; which will stop your repls turning to stone should you inadvertently look at the medusa
;; I have this set all the time!

(def r (java.util.Random. 1)) ; #'user/r
(defn reset-randomizer [n] (alter-var-root #'r (constantly (java.util.Random. n))))
(def #'clojure.core/rand (constantly (fn ([] (.nextDouble r)) ([n] (* n (rand))))))
(defn coin [] (rand-nth [:H :T]))

(reset-randomizer 1) ; #object[java.util.Random 0x42da3f20 "java.util.Random@42da3f20"] ; #object[java.util.Random 0x19d830ca "java.util.Random@19d830ca"] ; #object[java.util.Random 0x50e10fe1 "java.util.Random@50e10fe1"] ; #object[java.util.Random 0x338aedb2 "java.util.Random@338aedb2"] ; #object[java.util.Random 0x6f2c94cc "java.util.Random@6f2c94cc"]
(repeatedly 10 rand) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (0.3971743421847056 0.34751802920311026 0.29405703200403677 0.5064836273262351 0.11596708803265776 0.7705358800791777 0.65989270869342 0.15674689056984625 0.3782020453210553 0.13976268290375116) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (:H :T :H :T :T :H :T :H :T :T) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696)
(repeatedly 10 rand) ; (0.3971743421847056 0.34751802920311026 0.29405703200403677 0.5064836273262351 0.11596708803265776 0.7705358800791777 0.65989270869342 0.15674689056984625 0.3782020453210553 0.13976268290375116) ; (0.3971743421847056 0.34751802920311026 0.29405703200403677 0.5064836273262351 0.11596708803265776 0.7705358800791777 0.65989270869342 0.15674689056984625 0.3782020453210553 0.13976268290375116) ; (0.3971743421847056 0.34751802920311026 0.29405703200403677 0.5064836273262351 0.11596708803265776 0.7705358800791777 0.65989270869342 0.15674689056984625 0.3782020453210553 0.13976268290375116) ; (0.6949479796024919 0.8052277714737137 0.005025175992452557 0.523135155788333 0.7439844862373166 0.14202270321592614 0.481728301575598 0.5445548088936737 0.5771002613742765 0.20491354575856158) ; (0.3971743421847056 0.34751802920311026 0.29405703200403677 0.5064836273262351 0.11596708803265776 0.7705358800791777 0.65989270869342 0.15674689056984625 0.3782020453210553 0.13976268290375116) ; (:T :T :T :H :H :H :H :H :H :H) ; (0.3971743421847056 0.34751802920311026 0.29405703200403677 0.5064836273262351 0.11596708803265776 0.7705358800791777 0.65989270869342 0.15674689056984625 0.3782020453210553 0.13976268290375116)
(repeatedly 10 coin) ; (:T :H :H :T :H :H :T :T :T :T) ; (:T :T :T :H :H :T :H :T :T :H)
(repeatedly 10 coin) ; (:T :T :T :H :H :H :T :H :H :H) ; (:T :H :H :T :H :H :H :H :H :H)
(reset-randomizer 1) ; #object[java.util.Random 0xca96577 "java.util.Random@ca96577"] ; #object[java.util.Random 0x7650cc27 "java.util.Random@7650cc27"] ; #object[java.util.Random 0x9bdf6f9 "java.util.Random@9bdf6f9"] ; #object[java.util.Random 0x64703b1b "java.util.Random@64703b1b"] ; #object[java.util.Random 0x416ffc2a "java.util.Random@416ffc2a"] ; #object[java.util.Random 0x59b5b945 "java.util.Random@59b5b945"]
(repeatedly 10 rand) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (:T :H :H :T :T :H :T :T :H :H) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696)
(repeatedly 10 rand) ; (0.3971743421847056 0.34751802920311026 0.29405703200403677 0.5064836273262351 0.11596708803265776 0.7705358800791777 0.65989270869342 0.15674689056984625 0.3782020453210553 0.13976268290375116) ; (0.3971743421847056 0.34751802920311026 0.29405703200403677 0.5064836273262351 0.11596708803265776 0.7705358800791777 0.65989270869342 0.15674689056984625 0.3782020453210553 0.13976268290375116) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696) ; (:T :H :H :T :T :H :T :T :H :H) ; (0.7308781907032909 0.41008081149220166 0.20771484130971707 0.3327170559595112 0.9677559094241207 0.006117182265761301 0.9637047970232077 0.9398653887819098 0.9471949176631939 0.9370821488959696)
(repeatedly 10 coin) ; (:T :H :T :H :H :H :H :H :H :T) ; (:T :T :H :T :H :T :H :T :T :H)
(repeatedly 10 coin) ; (:T :H :T :H :T :T :T :T :H :H) ; (:T :H :T :H :T :H :H :H :T :T)
(reset-randomizer 2) ; #object[java.util.Random 0x19ba71a9 "java.util.Random@19ba71a9"]

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
([(:H :H :H :H :H :H :H :H :H) 1]
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

(map second (partition 2 (partition-by #(= % '-)  buses))) ; ((R G) (G) (G) (G) (R R R G) (R G) (G) (R G) (R R G) (R G) (G) (G) (R R G) (G) (R R G) (R G) (G) (G) (G) (R G) (R G) (R G) (G) (G) (R R R G) (G) (R G) (G) (G) (R G) (R G) (G) (G) (R R G) (G) (G) (R G) (R R R R R G) (G) (R G) (G) (R R R G) (G) (G) (G) (R R R G) (G) (G) (G) (G) (G) (R R G) (G) (G) (R G) (R G) (G) (R R G) (R R G) (G) (R R R R R R R R R G) (R G) (G) (G) (G) (G) (R R R G) (R R G) (R G) (R R R G) (G) (G) (G) (G) (G) (R G) (G) (G) (R R R G) (G) (G) (G) (G) (G) (R R R R G) (G) (R G) (G) (R R G) (R G) (R G) (G) (G) (R R G) (R R R G) (G) (R R G) (G) (R G) (R R R G) ...)


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

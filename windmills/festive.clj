#!/usr/bin/env clojure

;; A Festive Theorem

;; About a week ago, a friend of mine who is not noted for his interest in pure mathematics, and
;; whose identity I will conceal behind the alias "Sipper" caught me in the Wetherspoons and showed
;; me a number theory proof (or at least most of one, we couldn't actually finish the argument off)

;; I've never been interested in number theory. It seems both useless and boring, but the central
;; idea of this proof was intriguing, so today I sat down, again in Wetherspoons, with quite a lot
;; of paper and coffee and an all-day brunch (all for £7.30, I love Spoons, if you want to advertise
;; on this blog just get in touch), and hammered the damned thing out until I was happy with it.

;; The proof's really visual and beautiful, but not constructive, but it suggested an algorithm to
;; me, so I'm going to try to get that algorithm working, and then show why it works by visualizing
;; what it's doing.

;; I have no idea whether anyone will find this interesting, but since it's the only thing in all of
;; number theory that's ever struck me as worth knowing, and Sips found it interesting too, I'm going
;; to write it up.

;; In this first bit, a statement of the Theorem, which itself took me a while to get my head round.

;; make the repl stop after the first 20 elements of a sequence
(set! *print-length* 20)

;; If you square an even number, then it will be divisible by 4
;; But if you square an odd number, then it will leave a residue of 1 mod 4

(defn square [x] (* x x))
(def mod4 #(rem % 4))

(map mod4 (map square (range)))  ;; (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 ...)

;; If that's not obvious, think about it for a few minutes and see whether you can make it so....

;; This means that if you take an even and an odd number, and square them, and add the squares, then
;; the residue mod 4 will always be 1

(mod4 (+ (square 2)  (square 5)))     ;; 1    ( 2*2 + 5*5 = 29 = 28 + 1 = 4 * 7 + 1)
(mod4 (+ (square 10) (square 17)))    ;; 1    (etc)

;; Let's check this is actually true for lots of numbers:

(def naturals (map inc (range))) ;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)

(defn odd [n] (- (* 2 n) 1))
(def odds (map odd naturals))   ;; (1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 ...)

(defn even [n] (* 2 n))
(def evens (map even naturals)) ;; (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 ...)

;; Here are all possible pairs of natural numbers
(def pairs (for [sum naturals i (take (dec sum) naturals)] (list i, (- sum i)))) ;; ((1 1) (1 2) (2 1) (1 3) (2 2) (3 1) (1 4) (2 3) (3 2) (4 1) (1 5) (2 4) (3 3) (4 2) (5 1) (1 6) (2 5) (3 4) (4 3) (5 2) ...)

;; From which we can construct all odd-even pairs
(def odd-even-pairs (for [[i,j] pairs] (list (odd i) (even j)))) ;; ((1 2) (1 4) (3 2) (1 6) (3 4) (5 2) (1 8) (3 6) (5 4) (7 2) (1 10) (3 8) (5 6) (7 4) (9 2) (1 12) (3 10) (5 8) (7 6) (9 4) ...)

;; From which we can construct all sums of squares of odd and even numbers
(def odd-even-squares (for [[i,j] odd-even-pairs] (+ (square i) (square j)))) ;;(5 17 13 37 25 29 65 45 41 53 101 73 61 65 85 145 109 89 85 97 ...)

;; paranoid check, if I did that right they should all be 1 mod 4
(map mod4 odd-even-squares) ;; (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ...)


;; So, since both : it's obvious ; and also it looks like it might actually be true, I think we can take it to the bank that:

;; If o is an odd number, and e is an even number, then o*o+e*e is 1 mod 4

;; This has apparently been pretty obvious to everyone who's ever thought about it, and is just one
;; of those number-theory facts that always seem to fascinate people who aren't me.

;; But it occurred to Albert Girard in 1632 to wonder if the converse was true:

;; If you have a number that is 1 mod 4, can you always split it into odd and even squares?

;; Let's look for counterexamples:
(def early-ones (sort (take 10000 odd-even-squares))) ;; (5 13 17 25 29 37 41 45 53 61 65 65 73 85 85 89 97 101 109 113 ...)

(def candidates (map #(+ (* 4 %) 1) naturals)) ;; (5 9 13 17 21 25 29 33 37 41 45 49 53 57 61 65 69 73 77 81 ...)


;; This is such a hack, I am shame....
(require 'clojure.set)
(clojure.set/difference (set (take 100 candidates)) (set early-ones)) ;; (9 21 33 49 57 69 77 81 93 105 121 129 133 141 161 165 177 189 201 209 ...)

;; So it looks like the converse is not true.

;; 9, for instance, is 4*2+1, so it's a candidate, but it looks like you can't split it into odd and even squares.
;; Let's try by hand, just going through all the possible odd numbers

;; 9 = 1*1 + 8 ; 8 is not the square of anything
;; 9 = 3*3 + 0 ; 0 is not the square of a natural number

;; This seems a bit dodgy to me, 9 = 3 * 3 + 0 * 0, so does it work if we count 0 as an even number? This will turn out not to matter too much.....

;; Look at 21
;; 21 = 1*1 + 20 ; 20 is not the square of anything
;; 21 = 3*3 + 12 ; 12 is not the square of anything
;; 21 = 5*5 - 4  ; oops, 5*5 is too big, although it does look like 21 = 5*5 - 2*2, but that wasn't the question. Sum of squares, not difference of squares.

;; OK 33 then
;; 33 = 1*1 + 32 ; 32 is not the square of anything
;; 33 = 3*3 + 24 ; 24 is not the square of anything
;; 33 = 5*5 + 8  ;  8 is not the square of anything
;; 33 = 7*7 - 16 ; oh bloody hell, 33 = 7*7 - 4*4

;; Anyway, that's not the question. Neither 21 nor 33 are the sum of two squares., even though 21 = 5*4+1 and 33 = 4*8+1

;; Looking at the list of the ones that did work
early-ones ;; (5 13 17 25 29 37 41 45 53 61 65 65 73 85 85 89 97 101 109 113 ...)

;; One might notice that there are an awful lot of prime numbers on that list. (although they're not all primes....)

;; In fact
(defn divisors [n] (filter #(= 0 (rem n %)) (map inc (range n))))

(divisors 12) ;; (1 2 3 4 6 12)

(defn prime? [n] (= (count (divisors n)) 2))
(filter prime? (range 200)) ;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 ...)


(def candidate-primes (filter prime? candidates)) ;; (5 13 17 29 37 41 53 61 73 89 97 101 109 113 137 149 157 173 181 193 ...)

(clojure.set/difference (set (take 100 candidate-primes)) (set early-ones)) ;; #{}

(clojure.set/difference (set (take 1000 candidate-primes)) (set early-ones)) ;; #{}

;; If there's a prime of form 4*n+1 that isn't the sum of one odd and one even square, then it's fairly big

;; It looks as though, at least for the first thousand candidates, if a number is both prime, and of form 4*n+1,
;; then it is expressible as an even square plus an odd square.

;; Albert Girard, in 1632, presumably after a fair amount of dicking about with bits of paper,
;; conjectured that this might be so for all candidate primes.

;; On December 25th 1640, Pierre de Fermat wrote to his friend Marin Mersenne that he had proved
;; that this was true, but he did not trouble to include the proof or indeed ever tell anyone how he
;; did it.

;; On the 12th of April 1749, Leonhard Euler wrote to his friend Christian Goldbach that he had
;; proved that this was true, and then actually went and published his (intricate and difficult) proof.

;; In recognition of Fermat's invaluable contribution, this result is universally known as

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;             Fermat's Christmas Theorem.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let p be a prime number

;; Then p is expressible as the sum of odd and even squares
;; if and only if p is 4*n+1 for some natural number n









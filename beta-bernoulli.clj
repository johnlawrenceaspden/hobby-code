;; Mathematics as an experimental science

;; Beta-Bernoulli distribution / Machine Learning

;; I'm reading Kevin Murphy's Machine Learning: A probabilistic perspective.

;; As one of the exercises in chapter three, I've just proved that:

;; If you start with Coins which are biased, and the bias is chosen
;; uniformly (so that for instance a fair coin is just as likely as a
;; coin which comes up heads 1/3 of the time, or a coin which always
;; comes up heads).

;; And you pick out the fair ones by tossing them all 200 times, and
;; throw away all the ones that don't come up heads exactly 100 times
;; and tails 100 times.

;; Then, because your test, although probably about as rigourous as
;; you could reasonably get, has not guaranteed you fair coins.

;; You are ever so slightly more likely to see three heads in three
;; tosses than you would be if the coins were truly fair.

;; In fact the bias is now the same as if the coins had been chosen from the Beta(101,101) distribution

;; And so the chances of getting three heads in a row is not, as you might expect, one in eight,

;; But rather B(104,101)/B(101,101), where B is the Beta function

;; And that works out to be G(104)G(101)G(202)/G(205)G(101)G(101), where G is the Gamma function.

;; And that is 103!100!201!/204!100!100!, where ! is the factorial function

;; And that is (101*102*103)/(202*203*204)

;; And that is 

(/ (* 101 102 103) 202 203 204) ;-> 103/812

;; or 

(float (/ 103. 812)) ; 0.1268473

;; Which is to say, ever so slightly more than one in eight.

;; And I am inordinately pleased with myself, not for having proved
;; this result, which was easy, but for having worked out what the
;; wilderness of notation in the book actually means.

;; And it occurs to me that Mathematics is in fact an experimental
;; science, which makes definite predictions about physical things,
;; such as the movements of electrons in my computer when type the following things:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let us make coins with biases distributed uniformly between 0 and 1:

(defn make-coin []
  (let [theta (rand)]
    (fn [] (if (< (rand) theta) :h :t))))

;; Here is one:
(def a (make-coin))

;; Let us test it
(frequencies (for [i (range 200)] (a))) ;-> {:h 69, :t 131}

;; It is no good. To the scrapheap with it:
(ns-unmap *ns* 'a)

;; Let us instead make a large number of coins:

(def coins (for [i (range)] (make-coin)))

;; And throw away all the ones that do not satisfy our rigorous criterion
(def fair-coins
  (filter (fn[coin] (= {:t 100 :h 100 } (frequencies (for [i (range 200)] (coin))))) coins))

;; Let us toss each of our fair coins three times:
(def results (for [coin fair-coins] (frequencies (for [i (range 3)] (coin)))))

;; And add up all the results
(def collected-results (drop 1 (reductions (fn[m f] (assoc m f (inc (get m f 0)))) {} results)))

(take 10 collected-results) ({{:t 2, :h 1} 1} {{:t 2, :h 1} 2} {{:t 2, :h 1} 3} {{:t 2, :h 1} 4} {{:t 2, :h 1} 5} {{:h 2, :t 1} 1, {:t 2, :h 1} 5} {{:t 3} 1, {:h 2, :t 1} 1, {:t 2, :h 1} 5} {{:t 3} 1, {:h 2, :t 1} 2, {:t 2, :h 1} 5} {{:t 3} 1, {:h 2, :t 1} 2, {:t 2, :h 1} 6} {{:t 3} 1, {:h 2, :t 1} 3, {:t 2, :h 1} 6})

(def empirical-distribution (map 
                             (fn[m] [(float (/ (m {:h 3} 0) (reduce + (vals m))))
                                     (float (/ (m {:t 3} 0) (reduce + (vals m))))])
                             collected-results))

(take 10 empirical-distribution)

(doseq [e empirical-distribution] (println e))

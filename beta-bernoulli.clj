;; Mathematics as an experimental science

;; Beta-Bernoulli distribution / Machine Learning

;; I'm reading Kevin Murphy's Machine Learning: A Probabilistic Perspective.

;; As one of the exercises in chapter three, I've just proved that:

;; "For the Beta-Bernoulli model, the probability of the data is the
;; ratio of the normalizing constants of the prior distribution and
;; the posterior distribution". 

;; What I think this actually means is:

;; If you start with Coins which are biased, and the bias is chosen
;; uniformly (so that for instance a fair coin is just as likely as a
;; coin which comes up heads 1/3 of the time, or a coin which always
;; comes up heads).

;; And you pick out the fair ones by tossing them all 20 times, and
;; throw away all the ones that don't come up heads exactly 10 times
;; and tails 10 times.

;; Then, because your test has not quite guaranteed you fair coins,
;; but only coins which are quite a bit fairer than you started with:

;; You are ever so slightly more likely to see three heads (or three
;; tails) in three tosses than you would be if the coins were truly
;; fair.

;; In fact the bias is now the same as if the coins had been chosen
;; from the Beta(11,11) distribution.

;; And so the chances of getting three heads in a row is not, as you
;; might have naively expected, one in eight,

;; But rather B(14,11)/B(11,11), where B is the Beta function

;; And that works out to be G(14)G(11)G(22)/G(25)G(11)G(11), where G is the Gamma function.

;; And that is 13!10!21!/24!10!10!, where ! is the factorial function

;; And that is (11*12*13)/(22*23*24), where / and * need no introduction if you have got this far in the post.

;; And that is:

(/ (* 11 12 13) 22 23 24) ;-> 13/92

;; or 

(float (/ 13 92)) ; 0.14130434

;; Which is to say, slightly more than one in eight.

;; And I am inordinately pleased with myself, not for having proved
;; this result, which was easy, but for having worked out what the
;; mysterious squiggles in the book might actually mean in practice.

;; And it occurs to me that Mathematics is in fact an experimental
;; science, which makes definite predictions about physical things,
;; such as the movements of electrons in my computer when I type the
;; following things:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let us make coins with biases distributed uniformly between 0 and 1:

(defn make-coin []
  (let [theta (rand)]
    (fn [] (if (< (rand) theta) :h :t))))

;; Here is one:
(def a (make-coin))

;; Let us test it
(frequencies (for [i (range 20)] (a))) ;-> {:h 11, :t 9}

;; It is no good. To the scrapheap with it:
(ns-unmap *ns* 'a)

;; Let us instead make a large number of coins:
(def coins (for [i (range)] (make-coin)))

;; And throw away all the ones that do not satisfy our criterion
(def fair-coins
  (filter (fn[coin] (= {:t 10 :h 10 } (frequencies (for [i (range 20)] (coin))))) coins))

;; Let us toss each of our fair coins three times:
(def results (for [coin fair-coins] (frequencies (for [i (range 3)] (coin)))))

;; And add up all the results
(def collected-results (drop 1 (reductions (fn[m f] (assoc m f (inc (get m f 0)))) {} results)))

;; And calculate the empirical distribution of those results:
(def empirical-distribution (map 
                             (fn[m] [(float (/ (m {:h 3} 0) (reduce + (vals m))))
                                     (float (/ (m {:t 3} 0) (reduce + (vals m))))])
                             collected-results))

;; It does seem to me that after a while, these numbers settle down to something near 14%
(doseq [e empirical-distribution] (println e))


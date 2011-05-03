;; A Poisson Process

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Imagine a very lot of things, each of which has a very small chance of
;; happening, so that on average, only a few things happen every second.

;; A classic case is radioactivity, where we've got a very large number of
;; atoms, each of which has a very small chance of exploding in any given
;; second.

;; We might know the average number of decays per second without having the
;; slightest idea what the large number or the small chance are.

;; Alternatively, we might just know what the sample did over a one second
;; period, and be trying to guess what the constant is.

;; In probability, we model this situation with a Poisson process.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This cryptic function gives the probabilities, for a given decay rate l
;; of each of the numbers we might read on our Geiger counter.

;; I'm ashamed to say that although I wrote this function and can prove these
;; are the probabilities, I can't come up with an easy way to explain why it
;; gives the right answers. You have to imagine what the probabilities are for
;; larger and larger numbers but smaller and smaller chances, keeping the
;; average the same, and notice that they settle down as the numbers get
;; bigger. These are the limits:

(defn poisson [l]
  (map first
       (iterate
        (fn [[a c]] [(/(* a l) (inc c)) (inc c)])
        [(Math/exp (- l)) 0])))

;; So let's just take this as the definition of a Poisson process, and have a
;; look at what it means.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clojure has inherited common lisp's excellent format function, so by casting
;; one cryptic spell we can print out the probabilities for the first few geiger
;; counter readings, if the average decay rate is one click/second.

(clojure.pprint/cl-format nil "~{~$~^, ~}" (take 10 (poisson 1)))
;; "0.37, 0.37, 0.18, 0.06, 0.02, 0.00, 0.00, 0.00, 0.00, 0.00"

;; One way of looking at this is to say that if we ran the experiment one
;; hundred times, then we'd expect that 37 times we'd hear no clicks, 37 times
;; we'd get one click, 18 times we'd get two, 6 times we'd get three, and twice
;; we'd get four. Sort of. Although actually we'd be quite surprised if that
;; happened.

;; As a sanity check, let's work out what our expected number of clicks is here.
;; We know it should be one.

(reductions +
            (map *
                 (poisson 1)
                 (range)))

;; (0.0 0.36787944117144233 0.7357588823428847 0.9196986029286058 0.9810118431238463 0.9963401531726563 0.9994058151824183 0.999916758850712 0.9999897508033253 0.999998874797402 0.9999998885745216 0.9999999899522336 0.9999999991683892 0.9999999999364022 0.9999999999954802 0.9999999999997 0.9999999999999813 0.9999999999999989 0.9999999999999999 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 ...)

;; If we include enough terms we do indeed get one.

;; I enjoyed that. Let's try it again for a process with constant 7.5

(reductions +
            (map *
                 (poisson 7.5)
                 (range)))(0.0 0.0041481327761087525 0.0352591285969244 0.15192536292498304 0.44359094874512967 0.9904639221579046 1.810773382277067 2.83616020742602 3.934788948657041 4.964753393561123 5.823057097647858 6.46678487571291 6.905690178939081 7.180005993455438 7.338265117214875 7.4230467906574304 7.465437627378708 7.485308332091806 7.494074819465232 7.497727522537493 7.499169379013385 7.499710075191845 7.499903180969866 7.499969012485101 7.4999904792835475 7.499997187658062 7.499999200170416 7.499999780702826 7.499999941961828 7.4999999851562045 7.499999996327164 7.499999999119903 7.499999999795566 7.499999999953925 7.499999999989916 7.499999999997855 7.499999999999556 7.49999999999991 7.499999999999982 7.4999999999999964 7.499999999999999 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 ...)

;; Neat huh?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The philosophical foundations of probability theory are completely incoherent
;; as far as I can tell, so I've always felt free to believe whatever I like as
;; long as it means my intuition leads me to the right answers.

;; I've found that for me, intuition is best served by imagining that we have a
;; vast number of parallel universes, and in them the results of the experiments
;; are spread out in the proportions given by the formula.

;; It's fair to say that this position is controversial.

;; If we take the numbers to three significant figures:

(clojure.pprint/cl-format nil "~{~3$~^, ~}" (take 10 (poisson 1)))
;; "0.368, 0.368, 0.184, 0.061, 0.015, 0.003, 0.001, 0.000, 0.000, 0.000"

;; Then we'll notice that there are some very rare universes where we get five
;; clicks, or even six from a process where the average number of clicks/second
;; is one.

;; In fact none of the numbers is really zero. There will be some universes
;; where the whole radioactive sample decays at once.  It's probably best not to
;; be near the sample in those universes, but fortunately they're very rare.

(clojure.pprint/cl-format nil "~{~6$~^, ~}" (take 0 (poisson 1)))
;; "0.367879, 0.367879, 0.183940, 0.061313, 0.015328, 0.003066, 0.000511, 0.000073, 0.000009, 0.000001"

;; It turns out we need to sift through a million universes to have a good
;; chance of finding one where nine decays happen in one second. It would be a
;; cold day in hell before you found one where there were twenty.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Imagine that we're pointing a photon detector at a distant star, and we want
;; to make a guess at how bright it is.

;; Photons happening to hit your detector is very much the same sort of problem
;; as radioactive decays hitting your geiger counter.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's imagine that we've got an absolute stack of parallel universes.  In
;; some of them, the constant for the star is 1, in some it's 2, etc, right up
;; to 20, and those constants are all equally likely

;; We can calculate the proportions of those universes where we see all the
;; different numbers of photons.

;; Photons across the top, 0-9. Star types down the side, 1-20.

(clojure.pprint/cl-format nil "~{~{~$~^,~}~% ~}" (map #(take 10 (poisson %)) (range 1 21)))
;; 0.37,0.37,0.18,0.06,0.02,0.00,0.00,0.00,0.00,0.00
;; 0.14,0.27,0.27,0.18,0.09,0.04,0.01,0.00,0.00,0.00
;; 0.05,0.15,0.22,0.22,0.17,0.10,0.05,0.02,0.01,0.00
;; 0.02,0.07,0.15,0.20,0.20,0.16,0.10,0.06,0.03,0.01
;; 0.01,0.03,0.08,0.14,0.18,0.18,0.15,0.10,0.07,0.04
;; 0.00,0.01,0.04,0.09,0.13,0.16,0.16,0.14,0.10,0.07
;; 0.00,0.01,0.02,0.05,0.09,0.13,0.15,0.15,0.13,0.10
;; 0.00,0.00,0.01,0.03,0.06,0.09,0.12,0.14,0.14,0.12
;; 0.00,0.00,0.00,0.01,0.03,0.06,0.09,0.12,0.13,0.13
;; 0.00,0.00,0.00,0.01,0.02,0.04,0.06,0.09,0.11,0.13
;; 0.00,0.00,0.00,0.00,0.01,0.02,0.04,0.06,0.09,0.11
;; 0.00,0.00,0.00,0.00,0.01,0.01,0.03,0.04,0.07,0.09
;; 0.00,0.00,0.00,0.00,0.00,0.01,0.02,0.03,0.05,0.07
;; 0.00,0.00,0.00,0.00,0.00,0.00,0.01,0.02,0.03,0.05
;; 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.01,0.02,0.03
;; 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.01,0.01,0.02
;; 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.01,0.01
;; 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.01
;; 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00


;; The way I read this table is:

;; We had a look in several hundred parallel universes where we did the experiment.

;; In thirty seven of them, the star constant was one, and we saw no photons in our detector.

;; In another thirty seven, the star constant was one, but we saw one photon.

;; There were four universes where the star constant was five, and we saw nine photons.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So what should we believe if we point a photodetector at a star and detect 9
;; photons in one second?

;; Well, what we're trying to do is work out what the star constant is.

;; And what we know is that we're in one of the universes where we counted nine photons.

;; I.e. we know that we're in the last row of the table.

;; Let's have a look at that last row:

(clojure.pprint/cl-format nil "~{~$~^,~}" (map #(nth (poisson %) 9) (range 1 21)))

;; "0.00,0.00,0.00,0.01,0.04,0.07,0.10,0.12,0.13,0.13,0.11,0.09,0.07,0.05,0.03,0.02,0.01,0.01,0.00,0.00"

;; What is the total number of the universes that we could be in?
(reduce + (map #(nth (poisson %) 9) (range 1 21)))

;; 0.9963258860986628

;; Call it 100

;; Counting along, we see that there's about a 13% chance that we're in a
;; universe where the constant is 9

;; And about a 13% chance that the constant is 10

;; And a one percent chance that the constant is 4 and that was a particularly lucky second to measure.

;; And a one percent chance that the constant is 18 and it was a particularly bad second to measure in.

;; We might even want to say that there's an 88% chance that the constant is between 6 and 14.

;; Or a half chance that it's 8, 9, 10, or 11

;; As you might have guessed in advance, nine, the number of photons that we
;; actually saw in a second, is probably our best guess at the average number of
;; photons we'll see per second from now on. In classical statistics we'd call
;; that the maximum likelihood estimator, or an unbiased estimator, but in fact
;; there's only a 13% chance that it's the right answer.

;; Probably better just to keep the distribution above in mind.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What's the expected number of photons per second we'll see if we now run our
;; counter for a while?

;; Remember that we're assuming that stars come in discrete varieties 1-20
;; and that we saw 9 photons.

(defn expected-rate [ stars photons ]
  (/ 
   (reduce + (map *
                  stars
                  (map #(nth (poisson %) photons) stars)))
   (reduce + (map #(nth (poisson %) photons) stars))))

(expected-rate (map inc (range 20)) 9) ; 9.955123984139409

;; This strikes me as very weird.

;; We've seen 9 photons. Surely, given no more information than that, our best
;; guess should be that we'll see 9 photons/second?

;; Consider some edge cases.

;; What about if the stars are only ever of brightness one?

(expected-rate '(1) 9) ; 1.0

;; That's definitely right. We must have just been really lucky to catch nine
;; photons at once. Since we're *sure* that the star is brightness one, the long
;; term estimate has to be one.

;; Here we're in complete agreement with the maximum likelihood estimator, and
;; disagree violently with the unbiased estimator.

;; What about if stars come in brightnesses 1 or 10?

(expected-rate '(1 10) 9) ; 9.999927072835586

;; Again, fair enough. Chances are it was a brightness 10 star behaving
;; normally, but there's a very small chance that it was a brightness 1 star
;; having a very good second.

;; If we run our experiment often enough, the average value will be pulled down
;; a tiny bit by those cases.

;; What if we saw 5 photons?

(expected-rate '(1 10) 5) ; 9.325386911283335

;; Pretty much the same. But now we have to take more seriously the chance now
;; that it was a weak star having a field day rather than a bright star having a
;; half-hearted go.

;; If you're having to budget for photons for some reason, then 9.32 is an
;; unbelievably bad answer. Your budget is going to need to be 10 or 1 exactly.

;; But if you're placing a bet at a bookmaker's, or buying photon insurance from
;; a company with a presence in lots of parallel universes, then 9.33 might well
;; be a fair price. Everyone's got to make a profit.

(expected-rate '(1 10) 2) ; 1.1097148350481423

;; Same sort of thing. About one time in a hundred, two photons will have been
;; emitted by a brightness 10 star rather than a brightness 1 star. However that
;; occasional much brighter star will skew the results noticeably.

;; These answers all look about right to me:
(map (partial expected-rate '(1 10)) (range 20)) ; (1.001110551183876 1.0110931922809403 1.1097148350481423 1.9886759335192858 5.97152862794323 9.325386911283335 9.927658434429095 9.992713129077847 9.999270781535703 9.999927072835586 9.999992707230374 9.999999270722506 9.999999927072245 9.999999992707224 9.999999999270722 9.999999999927073 9.999999999992706 9.999999999999272 9.999999999999927 9.999999999999993)

;; The dodgy case is when we see 4 photons.  Brightness one stars have about a
;; 2% chance of producing 4 photons, and brightness ten stars have about a 2%
;; chance as well.

;; So half the time, we've got a 1, and half the time, we've got a 10.

;; So we're expecting to get a long term average of 10 half the time, and 1 the
;; other half.  And therefore we expect to see around 5.5 photons/second if we
;; add up all our experiments.

;; But I still find this weird:
(expected-rate (map inc (range 20)) 9) ; 9.955123984139409

;; However the whole range of photons that we can receive behaves correctly:

(map (partial expected-rate (map inc (range 20))) (range 40) ) ; (1.5819766656462542 2.163952866521654 3.0148662995404725 3.9961819317191947 4.9990309464433125 5.9993253251689005 6.997434541967013 7.992370962274987 8.980384871785288 9.955123984139409 10.90739733739242 11.825629164415346 12.697213993063654 13.510414954047969 14.256168445529575 14.92921796885468 15.52834070305211 16.055807374000462 16.516421661779617 16.916484374585746 17.262908595847506 17.562577608383947 17.821943140115458 18.04681608289834 18.242291371716 18.412756086521956 18.56194298618969 18.69300434170798 18.808590810798304 18.910926944365613 19.00187928605607 19.08301562443344 19.15565538916098 19.22091189359721 19.27972741592878 19.332902168984916 19.381118149636865 19.424958748228278 19.464924873069005 19.501448223241248)

;; If we get no photons at all, then we've probably got a low brightness star.
;; It'll most likely be a one, might well be a two, could be a three, and so on.
;; So we know the mean *must* be bigger than one.

;; Lots of photons is telling us that we've probably got a twenty, but might
;; have a 19, and could have an 18, so the mean should be less than 20.

;; That's enough evidence of correctness that I believe my calculation, even
;; though at first I was pretty sure that I'd made an off-by-one error
;; somewhere.

;; If anyone's still reading, can they see whether I got it right or not?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This way of calculating things is called Bayesian Statistics, after the
;; Reverend Thomas Bayes.  It's very general, but was considered to be just a
;; curiosity until computers came along and made it possible to do these
;; calculations on real problems.

;; Poisson processes are a nice simple case that I happened to be thinking about
;; as a result of reading David Mackay's beautiful book Information Theory,
;; Inference, and Learning Algorithms. The nice thing is that the techniques
;; work for any distributions or sets of models at all.

;; This half-witted way of thinking about what the calculations mean is my own,
;; and I don't want to blame it on Bayes or anyone else. But it works for me.















(reduce + (take 10 (poisson 1)))

(/ (* (Math/exp -9) (reduce * (repeat 9 9))) (reduce * (range 1 10)))

(clojure.pprint/cl-format nil "~{~$~^,~}" (map #(nth (poisson %) 9) (range 0 20 0.1)))


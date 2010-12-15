;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Occam's Razor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A problem from the wonderful:

;; Information Theory, Inference, and Learning Algorithms
;; by David J.C. MacKay

;; Which is available on-line as a pdf.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MacKay states the problem thus (in Chapter 28, Exercise 1):

;; Random variables x come from a probability distribution P(x).

;; According to model H0, P(x) is a uniform distribution

;; P(x | H0) = 1/2 for all -1 < x < 1

;; According to model H1, P(x) is a non-uniform distribution with a
;; parameter -1 < m < 1

;; P(x | H1, m) = ( 1 + m x ) / 2 for all -1 < x < 1

;; Given the data 0.3, 0.5, 0.7, 0.8, 0.9, what is the evidence for H0 and H1?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What on earth does this mean?

;; We're getting, from somewhere, a stream of numbers.

;; Those numbers are guaranteed to be between -1 and 1, but they can be any
;; values in that range.

;; We've got two theories about how those values are being generated.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In the first theory, H0, they're just being picked 'at random'. No number is
;; any more likely than any other number.

;; We'd be no more surprised by a -0.9 than by a 0.9, or a 0, or a 0.3.

;; Given our data:
(def data '(0.3, 0.5, 0.7, 0.8, 0.9))

;; We can calculate a likelihood P( D | H0 ), the probability of the data given H0.

;; In fact, since H0 says that any number is going to be as likely as any other
;; number, the likelihood will always be (1/2) raised to the power of the number
;; of data points.

;; We can't quite say probability here. The probability of any given number is
;; zero, because there are infinitely many possible numbers.  But we can talk
;; about the probability density function, which is the thing which tells you
;; how likely a number and lots of other numbers very near it are.

;; The pdf is always 1/2.

;; Handwavily speaking, we're saying that the probability of getting a number
;; between 0 and 0.000001 is 1/2 times 0.000001, which works out ok since the
;; numbers can go from -1 to 1, which is a range of 2.

;; Let's plot this simple constant probability density function (pdf)
(use 'simple-plotter)

(defn plot-pdf [title pdf]
  (create-window title 300 100 white black -1 1 0 1)
  
  (ink gray)
  (axes)
  (ink blue)
  (plot -1 (pdf -1))
  (doseq [x (range -1 1 0.01)]
          (draw-to x (pdf x))))

(plot-pdf "pdf for H0" (constantly 1/2))

;; Anyway:

(defn likelihood-given-H0 [data]
     (Math/pow 0.5 (count data)))


;; Here is a test for this function:
(likelihood-given-H0 data) ; 0.03125 (1 / 2^5, or 1/32 , or 0.03125)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In the second theory, the stream of random numbers is biased, but we don't
;; know exactly how.

;; We have to choose a parameter m for our second model, which can itself be
;; between -1 and 1 If m is high, then the second model prefers numbers that are
;; closer to 1

;; If m is 1, then the pdf looks like this:
(plot-pdf "pdf for H1, m=1" (fn[x] (/ (+ 1 (* x 1)) 2)))
;; And this means that you are twice as likely to get a number near 1 as a
;; number near 0, and numbers near -1 almost never happen

;; If, on the other hand, m were -1, then
(plot-pdf "pdf for H1, m=-1" (fn[x] (/ (+ 1 (* x -1)) 2)))
;; Representing a distribution where -1 is most likely, and 1 is very unlikely

;; In the middle, with m=0, then we just have the same model as H0 above
(plot-pdf "pdf for H1, m=0" (fn[x] (/ (+ 1 (* x 0)) 2)))

;; And there are intermediates
(plot-pdf "pdf for H1, m=-1/10" (fn[x] (/ (+ 1 (* x -1/10)) 2)))
;; Here is a distribution which slightly prefers negative numbers. But it might
;; take a while for an observer to notice.

;; We need a likelihood function for the second model. We'll get the likelihood as before,
;; by multiplying the values of the pdf at the data points:
(defn likelihood-given-H1_m [m data] (reduce * (map (fn[x] (/ (+ 1 (* x m )) 2)) data)))

;; With m=0 this had better be the same as above
(likelihood-given-H1_m 0 data) ; 0.03125 it is!

;; But the m=1 case, where the distribution is strongly biased towards the positive
;; is much more likely to produce our given data
(likelihood-given-H1_m 1 data) ; 0.354290625

;; and the m=-1 case is very unlikely to produce such results.
(likelihood-given-H1_m -1 data) ; .0000656249

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now Reverend Bayes tells us that we should first of all come up with some
;; prior probabilities for these two models. If we can do that, then he will
;; tell us how to adjust these probabilities as the data come in.

;; We have no idea, a priori, how likely one model is relative to the other.

;; I'm going to guess that in half the universes that exist, model H0 is true, and
;; in the other half, that model H1 is true. That seems fair-minded.

;; But H1 needs a parameter, m. So let's say that out of the half of the worlds
;; in which some version of H1 is true, the probability of m is evenly
;; distributed.

;; We can draw the models on a line:

;;                                       -1      -0.5        0          +0.5         +1
;; --------------------------------------++++++++++++++++++++++++++++++++++++++++++++++
;; Where the - represent worlds with H0 true, and the + represent worlds with H1 true,
;; and the m-values for the H1 worlds are written above.

;; Initially, when we have no samples, we'll give each of these worlds a
;; likelihood of 1

;; Let's draw that:

(defn plot-likelihoods-by-world [title likelihoodH0 likelihoodH1]
  (create-window title 300 100 white black -1 1 0 1)
  (ink gray)
  (axes)
  (ink yellow)
  ;; the likelihood of H0 is the same in all its worlds, so it will be a function of
  ;; no arguments
  (line -1 (likelihoodH0) 0 (likelihoodH0))
  ;; but the likelihood of H1 depends on m, which goes from -1 to 1
  ;; over the second half of the line
  (plot 0 (likelihoodH1 -1))
  (doseq [x (range 0 1 0.01)]
    (let [m (+ -1 (* 2 x))]
      (draw-to x (likelihoodH1 m)))))


(plot-likelihoods-by-world "no data yet" (constantly 1) (fn [m] 1))


;; When we get our first data point,
(let [partialdata (take 1 data)]
  (plot-likelihoods-by-world (apply str "after first data point:" partialdata)
                             (fn[]   (likelihood-given-H0 partialdata))
                             (fn [m] (likelihood-given-H1_m m partialdata))))

;; We see that the likelihood in the H0 worlds is constant, but the
;; likelihoods in the H1 worlds with a high m is higher, and in the H1 worlds
;; where m is low the likelihoods are low.

;; As a sanity check, we can see that the likelihood
;; in the H1 world where m is 0 (in the middle) is the same as in the H0 worlds.

;; If we're talking about whether the evidence favours H1 or H0, which were
;; initially equally likely, by our assumption, then we want to look at the
;; areas beneath the two curves.

;; They look about the same. This seems fair enough. The point 0.3 could easily have been
;; generated by the first model. The second with high m could have generated it slightly more easily, but the second with low m would have corresponding slight trouble.


;; Let's add in the second data point, and see how the picture changes
(let [partialdata (take 2 data)]
  (plot-likelihoods-by-world (apply str "after two points:"
                                    (clojure.string/join \, partialdata))
                             (fn[]   (likelihood-given-H0 partialdata))
                             (fn [m] (likelihood-given-H1_m m partialdata))))

;; Again, the areas under the curves are about the same, but it's clear that the H1 model
;; with a high m is preferred, possibly by a factor of about 3, to the H1 model with low m


;; Since all we care about is the relative area of the two curves, we might as well scale
;; the graphs up a bit. Both models have trouble making these exact predictions (as indeed would any model that doesn't predict them exactly). What we want to know is how they do relative
;; to one another.

(defn plot-scaled-likelihoods-by-world [title likelihoodH0 likelihoodH1]
  (let [yscale (max
                (reduce max (map likelihoodH1 (range -1 1 0.01)))
                (likelihoodH0))]
    (create-window title 300 100 white black -1 1 0 yscale))
  (ink gray)
  (axes)
  (ink yellow)
  ;; the likelihood of H0 is the same in all its worlds, so it will be a function of
  ;; no arguments
  (line -1 (likelihoodH0) 0 (likelihoodH0))
  ;; but the likelihood of H1 depends on m, which goes from -1 to 1
  ;; over the second half of the line
  (plot 0 (likelihoodH1 -1))
  (doseq [x (range 0 1 0.01)]
    (let [m (+ -1 (* 2 x))]
      (draw-to x (likelihoodH1 m)))))

(defn bayesdiagram [ data ]
  (plot-scaled-likelihoods-by-world (apply str "data:" (clojure.string/join \, data))
                             (fn[]   (likelihood-given-H0 data))
                             (fn [m] (likelihood-given-H1_m m data))))


;; Reprising the ones we've already seen, but scaled up a bit.

(bayesdiagram (take 0 data)) ;; with no data, we just get our priors. equal likelihood.
(bayesdiagram (take 1 data)) ;; H1 (high m) slightly preferred, H1 (low m) slightly dissed.
(bayesdiagram (take 2 data)) ;; more of the same

;; When we add in the 0.7, though:
(bayesdiagram (take 3 data))
;; It looks now as though the H1 curve has more area underneath it in total.
;; Even though the low-m H1 models now look very unlikely, high-m H1 seems to
;; be dominating the picture.

;; This is what we'd expect, I think. It's stretching it a bit to say that H0 produces three
;; positive numbers in a row, whereas we might very well expect that from H1 (high m).
;; However three high numbers are almost enough to rule out the low m H1 models. They'd be
;; much more likely to produce negative data.


;; Add in 0.8. H1 pretty clearly winning now!
(bayesdiagram (take 4 data))

;; And finally:
(bayesdiagram data)

;; It's clear that H1 has won this battle. The data we've seen tell us that we're probably
;; in a world where H1 is the true hypothesis, and m is high.

;; We've not ruled out H0, or indeed H1 with low m. We could have just got five unlucky samples.

;; But if, before we'd seen the data, we'd have taken or left a bet at evens on
;; H1 or H0, which is what our prior meant, then now we'd now be very reluctant to
;; bet on H0, and very enthusiastic to take that bet on H1.


;; How enthusiastic, though?

;; To work that out, we'd need to work out the areas under the two curves and compare them.
;; It looks about three to one to me. I leave the exact calculation to the reader.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; But remember what our data were! We were supposed to be generating numbers between
;; -1 and 1, and what we saw was the sequence 0.3, 0.5, 0.7, 0.8, 0.9 .

;; Just by intuition, we'd probably have figured that whatever was generating the sequence
;; was biased towards positive numbers.

;; Now we can put a quantity on our intuition: If we start off thinking that
;; either model is equally likely, then after seeing those five data points,
;; we'll think that H1, the biased model, is in the lead by about three to one.

;; And we also have a fair feeling for what sort of value of m we're going to
;; expect to find if we ever know the true process generating our data (and it
;; turns out to be one of our two candidates)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now let's look at some other sequences of data that we could have got.

;; Suppose that we'd only ever seen zeroes
(bayesdiagram '(0 0 0))

;; All our models are equally likely to generate zeroes. We don't have any new
;; information that lets us choose between one and the other.

;; If we're sure that they're the only two plausible models, then we've drawn a blank here.
;; We have the same estimates of their likelihood that we had before.

;; But we might well come up with another plausible model after looking at the data!

;; What about if we only see 0.5?
(bayesdiagram '(0.5 0.5 0.5 0.5 0.5))

;; Again, it's easy to think of another model that explains this well. But if we
;; are convinced for other reasons that one of H1 or H0 has to be true, then this
;; is a clear win for the high m version of H1.

;; What about if we see twenty -0.9s in a row?
(bayesdiagram (repeat 20 -0.9))
;; A landslide victory for low-m H1. No model except H1 with 0 < m < 0.25 is
;; even worth considering any more (out of our initial candidates with equal
;; weights).  Of course if we'd been convinced that H0 was the true model before
;; we started, then we might not believe that we've seen enough data to refute
;; it.

;; What we do know is that we should have vastly less confidence in it than we
;; did before.


;; That's about it for constant data. If the constant is a long way from 0, then we quickly
;; come to prefer a version of H1 to all other hypotheses. If it's close to 0
(bayesdiagram (repeat 20 -0.01))
;; then even twenty trials are not enough to make much of a difference. All our models
;; predict this sort of behaviour about equally.


;; What about data that actually looks to the intuition that it might have been
;; generated by H0?

(bayesdiagram '(0 -0.5 0.7))
;; This is new! The preferred model is now H1 with a slightly positive m.
;; This would classically be called the maximum likelihood estimator.

;; In all our previous experiments, the MLE was H1 with m = 1 or -1
;; But now the MLE is H1 with m about 0.3, just by looking at the graph.

;; However, it now looks as though we have moved from a position where we were initially
;; agnostic about the two hypotheses, to a position where we slightly prefer H0!

;; Even though H1(m ~ 0.3) explains the data best, it only does that for a narrow range of m.
;; Most of the versions of H1 actually do worse than H0.

;; So if we'd started out thinking for some reason that we had an even choice
;; between H0 and H1(with m ~ 0.3), we might now be starting to think that H1
;; was most likely.

;; But if we started out thinking that H1 and H0 were equal, but that we knew nothing about
;; the parameter m, we'd be rooting slightly for H0, by comparing the areas under the curves.


;; Let's look now at what we get by actually generating data using H0
;; This expression will generate 20 samples drawn evenly from -1 to 1, and
;; plot our diagram on the basis. 
(bayesdiagram (map (fn[x](- (rand 2) 1)) (range 20)))
(bayesdiagram (map (fn[x](- (rand 2) 1)) (range 100)))


;; Here are some sequences which were actually pulled from an H0 style generator
;; i.e. uniformly distributed over -1, 1, but truncated to two significant
;; figures for readability

(bayesdiagram '(0.98 0.97 0.93 0.75 -0.53 -0.95 0.97 -0.88 0.97 -0.94))
;; Here we see a clear victory for H0. The superior performance of the version of H1
;; which has been tuned for this exact data set is drowned out by the poor performance of
;; all the other H1s. Even though H1 (m~0.5) is the clear winner, we end up preferring H0
;; as a hypothesis.

;; Here's a generator for such a sequence of rounded samples
(defn H0samples [n]
  (map (fn[x] (/ (Math/round (* 100 (- (rand 2) 1))) 100.0)) (range n)))

;; And here are three more randomly generated sequences of ten.
(bayesdiagram '(-0.44 0.52 0.04 0.86 0.43 0.21 -0.52 -0.38 0.45 0.29))
(bayesdiagram '(-0.13 0.78 -0.11 0.1 -0.86 0.15 -0.15 -0.73 -0.63 -0.45))
(bayesdiagram '(-0.95 -0.83 -0.85 0.19 -0.75 -0.38 -0.34 0.2 0.93 0.32))
;; In all three, the hypotheses come out about even


;; What about with 20 samples?
;; Again, here are some sequences generated with (H0samples 20)

(bayesdiagram '(-0.11 0.91 0.02 0.74 -0.21 -0.14 0.58 -0.56 -0.97 -0.51 0.96 0.26 1.0 0.04 0.76 -0.23 -0.4 0.18 0.87 0.91))
;; H1 wins!

(bayesdiagram '(-0.38 0.36 -0.95 0.09 -0.37 -0.13 -0.61 0.89 0.63 -0.86 -0.87 0.67 0.54 -0.53 -0.23 -0.34 -0.94 -0.72 0.6 0.77))
;; H0 wins!

(bayesdiagram '(-0.52 -0.96 0.44 -0.08 -0.55 0.62 0.02 -0.39 -0.15 0.6 -0.98 -0.33 -0.95 0.63 -0.92 0.66 0.34 0.4 -0.04 -0.66))
;; about even

(bayesdiagram '(-0.96 0.01 0.42 -0.07 0.37 0.54 -0.32 0.71 0.37 0.55 0.26 0.73 -0.34 0.28 -0.98 -0.29 0.58 -0.42 0.1 -0.87))
;; H0 wins

;; There's not enough data here to determine the decision. What I think is going on is that even
;; a sequence of 20 samples will often look biased one way or another, and if it does, then
;; Bayes comes down on the side of the model that best represents that bias.

;; If, on the other hand, the data looks even (a bit too even to be true, in fact), which is to say
;; that it looks like what we'd expect a uniform distribution to generate, then Bayes decides for the uniform model.

;; FIX ME
;; What I'd like to do here is to throw a thousand samples in, and see what happens. But the program breaks for some reason.

;; In an attempt to throw some illumination on this subject, 

;; Here's some very extreme data. H1(m=0) is, of course, as good at explaining
;; this as H0, but all the other H1s have trouble with it.
(bayesdiagram '( 1 -1 0))
(bayesdiagram '( 1 -1 0 1 -1 0))
(bayesdiagram '( 1 -1 0 1 -1 0 1 -1 0))
(bayesdiagram '( 1 -1 0 1 -1 0 1 -1 0 1 -1 0))
(bayesdiagram (take 1000 (apply concat (repeat '(-1 0 1)))))


;; Here are twenty points exactly conforming to the H0 distribution (too good to be true!)
(bayesdiagram (range -1 1 0.1))
;; This diagram is what we'd expect if we used the method on a long series of
;; randomly generated data (so long that the histogram begins to approximate the
;; actual distribution)

;; here are 200
(bayesdiagram (range -1 1 0.01))

;; Here's a seriously positively biased sequence
(bayesdiagram '(0.1))
(bayesdiagram '(0.1 0.2))
(bayesdiagram '(0.1 0.2 0.3))
(bayesdiagram '(0.1 0.2 0.3 0.4 0.5 0.6))
(bayesdiagram (range 0 1 0.1))

;; For H1 with m=1, we can generate samples like:
;; x = -1 + 2*(sqrt (rand))

(defn H1samples [n]
  (map (fn[x] (/ (Math/round (* 100 x)) 100.0))
       (map (fn[x] (+ -1 (* 2 (Math/sqrt (rand))))) (range n))))

;; It appears that 20 samples from the H1(m=1) distribution are almost always enough
;; to convince our method that H1 is the true hypothesis:
(bayesdiagram (H1samples 20))

;; Which is emphatically not the case for H0 samples, which may or may not appear to be H1 samples
(bayesdiagram (H0samples 20))
;; However we do notice here that in the cases where H1 wins, it is always a version of H1 with a strong bias.
;; Some H1-type hypothesis is almost always the maximum likelihood estimator!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Brief mathematical aside on how to generate H1 samples

;; How would we generate samples for general H1?
;; We have to integrate the pdf to find the cumulative frequency function
;; P(x) = 1/2 (1+mx)
;; integrates to 1/2 (x + m x^2/2) + c
;; We want the cumulative frequency at -1 to be zero, which lets us choose
;; the integration constant:
;; i.e. we want c such that 1/2 (-1 + m /2) +c = 0
;; c = 1/2-m/4

;; The cumulative frequency function for H1(m) is therefore 
;; 0.25 m x^2 + 0.5 x + 0.5 - 0.25 m

;; so if we can generate a random number between 0 and 1, then solve
;; 0.25 m x^2 + 0.5 x + (0.5-0.25m-rand) = 0 for the root between -1 and 1
;; then we have the distribution.

;; using the quadratic formula
(defn makeH1sampler [m]
  (fn []
    (let [a (* 0.25 m)
          b 0.5
          c (- 0.5 (* 0.25 m) (rand))]
      (/ (+ (- b) (Math/sqrt (- (* b b) (* 4 a c)))) (* 2 a)))))

(defn H1samples [m n]
  (if (= m 0) (H0samples n) ; m zero is a special case, but we can use the H0 generator
      (let [sampler (makeH1sampler m)] 
        (map (fn[x] (sampler)) (range n))))) 

;; Here are some tests that suggest we got it right!

(reduce max (H1samples -1 100)) ; 0.7900622011350507
(reduce min (H1samples -1 100)) ; -0.9965276294448893

(reduce max (H1samples -0.5 100)) ; 0.9979112000065367
(reduce min (H1samples -0.5 100)) ; -0.9899445379410134

(reduce max (H1samples -0.1 100)) ;0.9846901232143168
(reduce min (H1samples -0.1 100)) ;-0.9941005477830345

(reduce max (H1samples 0.5 100)) ;0.9890561002476748
(reduce min (H1samples 0.5 100)) ;-0.9790371895713657

(reduce max (H1samples 1 100)) ; 0.9909238165578513
(reduce min (H1samples 1 100)) ; -0.7313143327689071

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(bayesdiagram (H1samples 1 20))   ;; H1 usually wins by miles
(bayesdiagram (H1samples 0.5 20)) ;; H1 often wins, but sometimes not
(bayesdiagram (H1samples 0.3 20)) ;; H1 sometimes wins, but sometimes H0 wins by razor
(bayesdiagram (H1samples 0.1 20)) ;; H1 sometimes wins, but sometimes H0 wins by razor


;; Sequences of 20 observations aren't really enough to decide the issue. I should rephrase the whole thing in terms of log-likelihoods or bigdecimals



(bayesdiagram (H1samples 0.1 500)) ;; H1 sometimes wins, but sometimes H0 wins by razor
;;1150 samples breaks this and I don't know why
































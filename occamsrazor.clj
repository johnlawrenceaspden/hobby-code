;; Occam's Razor

;; The principle of choosing the simplest model that fits the facts is given
;; quantitative form in Bayesian Statistics.

;; It seems intuitively rather dishonest to create a model with parameters, then
;; to pick the parameters so that they fit the data, and then to declare that
;; that model is best.

;; After all, taken far enough, that results in a model which always generates
;; the exact data we've seen. Such a model is often intellectually unsatisfying
;; and is unlikely to generalize well.

;; It's that sort of over-tuning that Occam's Razor warns us to avoid, and that's
;; why people once believed that the all the planets go round the Sun according
;; to the Copernican model of ellipses.

;; The older theory was that the other planets and the Sun go round the Earth
;; according to a complicated arrangement of epicycles.

;; If anything, in Copernicus' time, the epicyclic model, already carefully
;; tuned, fit the observed data rather better than the heliocentric
;; model. Copernicus' model won because it seems strongly intuitively better,
;; needing much less tuning and coincidence to model the observed data.

;; Of course neither model is correct.

;; A better model was later given by Newton's laws with even fewer parameters,
;; and later still by Einstein's geometrodynamics, which actually has slightly
;; more parameters than the Newton model, and which won out over Newton's laws
;; because of its better correspondence with observations.

;; As we've adopted these models in sequence, we've simultaneously gained more
;; and more ability to predict the future movements of the planets, and more and
;; more ability to generalize our theories to more distant objects and exotic
;; situations.

;; We know that General Relativity is not the truth. We have not yet even
;; managed to think of a model which could explain all the relevant data, but
;; there does seem to be a strict sense in which it is closer to the truth than
;; any of the previous models.

;; To generalize from this one example, it seems that we should generally prefer
;; models with fewer parameters, but models with more parameters can be
;; preferred when they produce better correspondence with reality.

;; How does Bayes tell us to choose?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's a very simple demonstration of how the Bayesian viewpoint automatically
;; compensates for the extra complexity of a model.

;; (Exercise 28.1 from Information Theory, Inference and Learning Algorithms, by David Mackay)

;; Here's an example data vector
(def data [ 0.1 0.3 0.5 0.7 0.9 ])

;; Note that the vector on its own tells us nothing about the process that generates the data.
;; It might be 'start at 0.1 and increase by 0.2 every time'.

;; In order to make inferences, we have to make assumptions:

;; Let's imagine that we're playing a game against an adversary.

;; He has two models which he calls H0 and H1.

;; In the first model (H0), the chance of getting a number between -1 and 1 is uniformly
;; distributed. P(x|H0) is 1/2 for all x in that range.

(defn h0[x] (if (<= -1 x 1) 1/2 0))

(h0 0.1) ; 1/2

;; In the second model, or family of models (H1), a parameter is needed. Here's
;; a function which given an m produces a model. m must be between -1/2 and 1/2.

(defn h1 [m]
  (fn[x] (if (<= -1 x 1) (+ 1/2 (* m x)) 0)))

;; Note that H1(m=0) is just H0
((h1 0) 0.5) ; 0.5
;; But H1(m=0.5) has a higher chance of producing higher numbers
((h1 0.5) 0.5) ; 0.75
;; and a correspondingly smaller chance of predicting lower ones
((h1 0.5) -0.5) ; 0.25

;; The adversary decides which model to use by tossing a coin. If he chooses the
;; model with the parameter, then he randomly chooses the parameter from its
;; range as well.

;; He then generates a set of data according to the model he's selected, and
;; asks us to tell him which model he used.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Of course, we can't. If both models would have been capable of generating the
;; data, then either could have actually done.  But the data do tell us
;; something. Given our assumptions and the data, we can calculate the
;; probability of each model.

;; Sometimes, for some data, both models will be equally likely.  Sometimes the
;; data will indicate either weakly or strongly that one model is more likely to
;; have been used.

;; For instance, if there's lots of data evenly spread out over (-1,1), then we
;; can guess that the model must have been either H0, or H1 with the parameter m
;; very close to 0.

;; Since the adversary has a half-chance of choosing H0, but only a small chance
;; of choosing H1 and then choosing a small parameter, it's clear that we should
;; bet on the actual model being H0.

;; Alternatively, if we have lots of data but it's distributed so that the mean
;; value is well away from 0, then we can guess that it is much more likely that
;; the adversary picked H1 and a certain m, than that he picked anything else
;; and got a freakishly unlikely set of data with a strange mean.

;; These inferences are easy to make if we have a lot of data. But Bayes'
;; Theorem allows us to calculate precise probabilities for each model, telling
;; us how much trust we should place in our intuition, and allowing us to see
;; exactly when our data is sending a strong enough signal to believe.

;; The more data we have, the more our calculation will favour one model rather
;; than the other.

;; If we for some reason get an opportunity to place bets on our predictions,
;; we'll know the odds.  Although of course that doesn't allow us to bet with
;; our adversary, or anyone else who actually knows the answer!

;; The probability of any of these models generating a data vector is given by
;; multiplying together the probabilities of each individual point.

(defn probability [model data]
  (reduce * (map model data)))

;; For any five point data vector which is in the right range, h0 always gives
;; us a probability of 1/2^5. It's equally likely to generate all the possible
;; data sets.

(probability h0 data) ; 1/32

;; For H1, the situation is dependent on the parameter. The sample data is clearly biased towards the positive end of its range:

;; It's very unlikely to have been generated by a low-biased version of H1
(probability (h1 -1/2) data) ; 2.953125E-4
;; Much more likely to have been generated by the unbiased version
(probability (h1 0) data) ; 0.03125
;; And ten times more likely still to be generated by the most +ve biased version of our model.
(probability (h1 1/2) data) ; 0.21651093750000003


;; The evidence for H0, is the probability, given that H0 is the model, of the data
(defn evidenceh0 [data]
  (probability h0 data))

(evidenceh0 data) ; 1/32

;; The evidence for H1, is the probability, given that H1 is the model, of the data.
;; This is complicated by the variable parameter. Really, we should integrate, but we can
;; approximate the integral by averaging over a hundred submodels between m=-1/2 and m=1/2

(defn evidenceh1 [data]
  (* 0.01 (reduce + (map #(probability (h1 %) data) (range -1/2 1/2 0.01)))))

(evidenceh1 data) ; 0.05518837559685008

;; That's about 1/20, versus 1/32 for the simple model.
;; the extra explanatory ability of the model with the parameter is being compensated for
;; by the unlikeliness of choosing a parameter which makes the data much more likely.

;; What we're interested in the the ratio between these two quantities:

(defn evidenceratio [data]
  (/ (evidenceh1 data)
     (evidenceh0 data)))

(evidenceratio data) ; 1.7660280190992026

;; If we see data like our data, it's about 17:10 that the model the adversary is using is H1

;; In this case, we can't really decide. But probably about two times out of
;; three times when we see similar data, we'll find that the model used was H1
;; (and it will usually have a high parameter)

(evidenceratio [0.1 0.3 0.5 0.7 0.9]) ; 1.7660280190992026

;; On the other hand, if the data are spread out, the simplicity of the H0 model
;; makes it slightly more likely.
(evidenceratio [-1 -0.5 0 0.5 1.0]) ; 0.6332833319999994

;; If we see horribly unlikely (but possible) data like:

(evidenceratio [-1 -1 -1 -1 -1  1 1 1 1 1]) ; 0.36940836940025096

;; The H0 is about 3 times more likely to have been responsible.

;; For a more even spread, which we'd intuitively expect to be strong evidence for H0:
(evidenceratio [-1 -0.75 -0.5 -0.25 0 0.25 0.5 0.75 1.0]) ; 0.5606379733249687

;; We in fact get no real evidence either way.

;; But note that most of the points in this range are explicable either by H0 or
;; equally by many different types of H1. The simultaneous presence of -1 and 1
;; is ruling out some of the extreme H1 models, but the rest of the data is
;; fairly ambivalent, and so we don't learn much at all.

;; This is telling us that our initial intuition, that it was the mean of the
;; data that we should be looking at, although helpful, is not the full story.

;; If the data's all up one end of the range then it's very very much more
;; likely that the data was generated by H1 with an appropriate parameter:

(evidenceratio [1 1 1 1 1 1 1 1 1 1]) ; 88.05623218526695

;; If all the data's in the middle, then we know nothing at all!
(evidenceratio [0 0 0 0 0 0 0 0 0 0]) ; 1.0

;; All models are equally likely to produce this kind of data.

;; Of course, if we really saw any of the above data sets, we'd probably look
;; for models which produce round numbers. That's ok. We can plug them into the
;; same framework.

;; We'll find they're much better than either H0 or H1. But they pay for their
;; specificity. A single observation of a not-round number will destroy their
;; power, and hand all the probability back to less tuned models like H1 and H0.





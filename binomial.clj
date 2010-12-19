;; The Binomial Distribution

;; Example 1.1 of ItILA

;; A bent coin has probability f of coming up heads. The coin is tossed N times.
;; What is the probability distribution of the number of heads, r? What are the
;; mean and variance of r?

;; Suppose f is 1/3, and we toss the coin once

;; 1/3 of the time r is 1, and 2/3 of the time, r is 0

;; If we toss it twice, then those probabilities split

;; 1/3 -> 1/3 * 1/3 and 1/3 * 2/3
;; 2/3 -> 1/3 * 2/3 and 1/3 * 2/3

;; So there are four separate sets of universes

;; In 1/9 ths of them, the coin came up heads twice
;; In 2/9 ths of them, the coin came up heads and then tails
;; In 2/9 ths of them, the coin came up tails and then heads
;; In 4/9 ths of them, the coin came up heads then heads

;; Ignoring the order, then we can unite the two 2/9ths possibilities to say that in 4/9ths
;; of the possible worlds, the coin has come up heads once.

;; So, 1/9 of the time r is 2, 4/9ths of the time it is 1, 4/9ths of the time it is 0

;; P(2)=1/9
;; P(1)=4/9
;; P(0)=4/9

;; If we call f 1/3 and g = 2/3 = 1-f
;; Then P(2)=f P(1) + g P(2) from the previous distribution

;; Let us call P(r, N, f) the probability of r heads in N trials given
;; probability f of a head in a single trial.

;; We see a recursion. What do we know so far?

;; P(0,0,f) = 1
;; P(r,0,f) = 0 for any r <> 1

;; P(0,1,f) = 1-f
;; P(1,1,f) = f

;; P(0,2,f) = (1-f)*P(0,1,f)+ [ f*P(-1,1,f) ]
;; P(1,2,f) = (1-f)*P(1,1,f)+f*P(0,1,f)
;; P(2,2,f) = [(1-f)*P(2,1,f) ]+ f*P(1,1,f)

;; That's enough information to write the recursion:

(defn P[r,N,f]
  (if (= N 0) (if (= r 0) 1 0)
      (+ (* f (P (dec r) (dec N) f)) (* (- 1 f) (P r (dec N) f)))))

;; since it's a tree recursion we'll memoize it
(def P (memoize P))

;; Test it by seeing whether it reproduces our old knowledge
(P -1 0 1/3) ; 0
(P  0 0 1/3) ; 1
(P  1 0 1/3) ; 0

(P -1 1 1/3) ; 0N
(P  0 1 1/3) ; 2/3
(P  1 1 1/3) ; 1/3
(P  2 1 1/3) ; 0N

(P -1 2 1/3) ; 0N
(P  0 2 1/3) ; 4/9
(P  1 2 1/3) ; 4/9
(P  2 2 1/3) ; 1/9
(P  3 2 1/3) ; 0N

;; And now try to create some new knowledge
(P -1 3 1/3) ; 0N
(P  0 3 1/3) ; 8/27
(P  1 3 1/3) ; 4/9
(P  2 3 1/3) ; 2/9
(P  3 3 1/3) ; 1/27
(P  4 3 1/3) ; 0N

;; What about an explicit formula?

;; If we have a sequences of H and T which are N long, how many have r H's?

;; The answer is N choose r, by definition. Each one has probability f^r*(1-f)^(N-r)

;; But how to calculate N choose r?

;; Well, if we take all the sequences of length 2, and add H, and all the
;; sequences of length 2 and add T, then we'll have all the sequences of length
;; 3.

;; And if we know 2 choose r for all r, then we can see that 3 choose r must be 2 choose r + 2 choose r-1, by the recursion above.

;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1

;; etc

;; What would an explicit formula look like?

;; The total number of sequences doubles at every pass.

;; How many different ways are there of mixing 3 H and 2 T?

;; We have 5! permutations of a b c d e

;; If we say that a=b=c=H and d=e=T, how many of these 5! permutations are the same?
;; Ah! we can permute a b c and we can independently permute d e without affecting the
;; sequence, so each sequence must have 3!2! aliases in the 5! permutations

;; Thus we may define

(defn factorial [n] (if (= n 0) 1 (* n (factorial (dec n)))))

(defn choose [N r]
  (/ (factorial N) (factorial (- N r)) (factorial r)))

;; or

(defn ways [heads tails]
  (/ (factorial (+ heads tails))
     (factorial heads) (factorial tails)))

(defn power [a n]
  (if (= n 0) 1 (* a (power a (dec n)))))

(defn probability [heads tails f]
  (* (power f heads) (power (- 1N f) tails) (ways heads tails)))



(probability 2 0 1/3) ; 1/9
(probability 1 1 1/3) ; 4/9
(probability 0 2 1/3) ; 4/9

(probability 10 5 1/3) ; 32032/4782969




;; But anyway we are going to define

(defn binomial [r f N] = (* (choose N r) (power f r) (power (- 1 f) (- N r))))

(binomial 1 1/2 3) ; 3/8 chance of 1 head in three tosses of a fair coin
(binomial 4 1/2 9) ; 63/256 chance of 4 heads in nine tosses of a fair coin

;; What is the mean of the binomial distribution?

(defn binomial-mean [f N]
  (reduce + (map (fn [r] (* r (binomial r f N) )) (range 0 (inc N)))))

(binomial-mean 1/2 10) ; 5N hooray!
(binomial-mean 1/3 10) ; 10/3 

;; Postulate on intuitive grounds that the mean of the binomial distribution is N*f

;; Indeed it seems that this is so.

(binomial-mean 1/3 2) ; 2/3
;; 2 * 1/9 + 1 * 4/9 + 0 * 4/9 = 2/9 + 4/9 = 6/9 = 2/3    1/3 chance, 2 tosses

;; But why?

;; Let's look at a single toss of a fair coin

;; P(1) = 1/2 P(0) = 1/2
;; Expectation = 1 * 1/2 + 0 * 1/2 = 1/2

;; Two tosses

;; P(0) = 1/4 P(1) = 1/2 P(2) = 1/4
;; Expectation = 0*1/4 + 1*1/2 * 2* 1/4 = 1/2 + 1/2 = 1

;; or alternatively    1/2 * ( 0 * 1/2 + 1 * 1/2)
;;                  +  1/2 * ( 1 * 1/2 + 2 * 1/2)

;;                     2* 1/2 * (0 * 1/2 + 1 * 1/2) 



;; What about generating samples from this distribution?

(defn binomial-sample [f N]
  (if (= N 1)
    (if (< (rand) f) 1 0)
    (+ (binomial-sample f 1) (binomial-sample f (dec N)))))

(binomial-sample 0.3 10)

(defmacro sample-seq [sexp]
  `(map #(%) (repeat (fn[] ~sexp))))

(def myseq (sample-seq (binomial-sample 0.3 10)))

(defn seq-sums [sq] (reductions (fn [a x] (+ a x)) 0 sq))
(defn seq-sqsums [sq] (reductions (fn [a x] (+ a (* x x))) 0 sq))
(defn scaledown [sq] (map / sq (drop 1 (range))))

(def samplemeans (map float (scaledown (seq-sums myseq))))
(def samplesqmeans (map float (scaledown (seq-sqsums myseq))))

(def ssq-smeans (map - samplesqmeans (map #(* % %) samplemeans)))

(take 10 (drop 100000 ssq-smeans))


     

(defn sample-mean [sq]  (/ (reduce + sq) (count sq)))
(defn sample-var [sq]
  (let [sm (sample-mean sq)]
    (/ (reduce + (map #(* (- % sm) (- % sm)) sq)) (count sq))))


(sample-mean (take 100 (map #(* % %) (map #(- % 3) myseq))))

;; Sample mean should head for 3 = 10 * 0.3
(map float (map #(sample-mean (take % myseq)) (drop 1 (range))))

;; Sample variance should head for 2.1 = 10 * 0.3 (1 - 0.3) = 3 * 0.7
(map float (map #(sample-var (take % myseq)) (drop 1 (range))))

(* 10 0.3 (- 1 0.3))

(sample-mean (take 100 myseq))
  














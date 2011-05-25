;; Numerical Integration: Harder Functions

;; So far we've found a couple of approximations to 'the area under a graph'.

(defn trapezium-rule [f a b]
  (* 1/2 (- b a) (+ (f a) (f b))))

(defn simpson-rule [f a b]
  (let [midpoint (+ a (/ (- b a) 2))]
    (* 1/6 (- b a) (+ (f a) (* 4 (f midpoint)) (f b)))))

;; And a way of repeatedly splitting intervals and applying the rules to the sub-intervals
;; to produce better and better approximations.
(defn iterated-rule [rule f a b N]
  (if (= N 0)
    (rule f a b)
    (let [midpoint (+ a (/ (- b a) 2))]
      (+ (iterated-rule rule f a midpoint (dec N))
         (iterated-rule rule f midpoint b (dec N))))))

;; We know that Simpson's rule is exact for our original function

(defn square[x] (* x x))

(simpson-rule square 0 2) ; 8/3
(iterated-rule simpson-rule square 0 2 1) ; 8/3
(iterated-rule simpson-rule square 0 2 2) ; 8/3

;; How do we do with some other functions?

;; Another function where we can calculate the answer directly is sine
(defn sine [x] (Math/sin x))

;; The integral of sine over a half-turn (i.e. over the interval [0,pi]) is exactly 2.

;; here are the first few approximations with the trapezium rule:
(take 10 (map (partial iterated-rule trapezium-rule sine 0 Math/PI) (iterate inc 0)))
;; (1.9236706937217898E-16 1.5707963267948968 1.8961188979370398
;; 1.9742316019455508 1.993570343772339 1.9983933609701445 1.9995983886400377
;; 1.9998996001842024 1.9999749002350526 1.9999937250705755)

;; After an inauspicious start, the answer is clearly settling down to 2.

;; Simpson's rule also settles down to 2, but it's much quicker.
(take 10 (map (partial iterated-rule simpson-rule sine 0 Math/PI) (iterate inc 0)))
;; (2.094395102393196 2.0045597549844216 2.000269169948388 2.0000165910479364
;; 2.000001033369414 2.0000000645300027 2.000000004032258 2.000000000252003
;; 2.000000000015751 2.000000000000985)

;; So it looks like we're onto a winner so far.

;; Another type of function that doesn't do so well is:

(defn step [x]  (if (< x 1/2) 0 1))

;; It should be fairly obvious that the integral of this over [0,1] is 0.5, but
;; in fact the convergence of the methods is very slow compared to what we had
;; for sine or square.

(take 10 (map (partial iterated-rule trapezium-rule step 0. 1) (iterate inc 0)))
;; (0.5 0.75 0.625 0.5625 0.53125 0.515625 0.5078125 0.50390625 0.501953125 ...

(take 10 (map (partial iterated-rule simpson-rule step 0. 1) (iterate inc 0)))
;; (0.8333333333333336 0.5833333333333335 0.5416666666666667 0.5208333333333335
;; 0.5104166666666667 0.5052083333333335 0.5026041666666667 0.5013020833333335
;; 0.5006510416666667 0.5003255208333335 )

;; Notice how if we make the tiniest possible change to the function, which doesn't change the integral at all:
(defn evil-step [x]  (if (<= x 1/2) 0 1))

;; the approximations are all changed quite a lot, although they do seem to
;; still converge to the correct answer.

(take 10 (map (partial iterated-rule trapezium-rule evil-step 0. 1) (iterate inc 0)))
;; (0.5 0.25 0.375 0.4375 0.46875 0.484375 0.4921875 0.49609375 0.498046875 0.4990234375)
(take 10 (map (partial iterated-rule simpson-rule evil-step 0. 1) (iterate inc 0)))
;; (0.1666666666666667 0.4166666666666668 0.4583333333333335 0.4791666666666668
;; 0.4895833333333335 0.4947916666666668 0.4973958333333335 0.4986979166666668
;; 0.4993489583333335 0.4996744791666668)



;; What about if we're integrating a (moderately) badly behaved function?:

(defn inverse [x] (/ x))

;; the real answer, by devious mathematical trickery is (log a) - (log b)

;; So if we integrate over the region [0.0001, 1], where the values of 1/x are
;; sometimes very large, we should get:

(- (Math/log 1) (Math/log 0.0001)) ; 9.210340371976182

(take 10 (map (partial iterated-rule trapezium-rule inverse 0.0001 1) (iterate inc 0)))
;; (4999.99995 2500.9997750199977 1251.8327765203333 627.5916420975113
;; 315.8156999790102 160.27209317742054 82.84288624590312 44.46707207824598
;; 25.61044440290146 16.499072595032356)

(take 10 (map (partial iterated-rule simpson-rule inverse 0.0001 1) (iterate inc 0)))
;; (1667.9997166933313 835.4437770204453 419.5112639565708 211.89038593950997
;; 108.42422424355732 57.03315060206397 31.67513402236028 19.324901844453297
;; 13.461948659075995 10.812578055293251)

;; It looks like both the rules, after starting off with appalling first
;; guesses, are converging to something, and maybe even to the right answer, but
;; the convergence is very slow.  Remember that to get the tenth element of this
;; sequence we're splitting the interval up into 1024 pieces, and we haven't
;; even got the answer right to one significant figure.

;; Numerical Integration can give very misleading answers if it's used naively.
;; Quite often, the results of a careless numerical simulation will just 'look
;; wrong' to a trained eye, and in those cases it's usually the eye that's in
;; the right.

;; One way to be reassured that your answer is roughly correct is to alter
;; expected degree of accuracy of the method, and to check that the answer
;; doesn't change by much!































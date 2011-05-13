;; Numerical Integration

;; Part I, What is an integral, anyway?

;; What does it mean to evaluate the integral of a function over an interval?

;; Let's take an example function

(defn square[x] (* x x))

;; We could evaluate it at a few input values:
(map square [0 1 2]) ; (0 1 4)

;; The integral of a function over an interval is a sort of
;; 'average sum over all the possible values in the interval'.

;; An first approximation to the integral of square over the interval [0,2]
;; Is to add the value at 0 to the value at 1

(reduce + (map square [0 1])) ; 1

;; And another is to add the value at 1 to the value at 2

(reduce + (map square [1 2])) ; 5

;; These answers are quite different! One is too low, because we paid too much attention
;; to the numbers at the start of the interval. One is too high, because we added up numbers
;; from the end.

;; We can make them both answers more accurate by sampling the function at more points, and
;; dividing the sum by an appropriate factor to make up for having more points.

(/ (reduce + (map square [0 1/2 1 3/2])) 2) ; 7/4
(/ (reduce + (map square [1/2 1 3/2 1])) 2) ; 9/4

;; We can continue the procedure

(/ (reduce + (map square [0 1/4 1/2 3/4 1 5/4 6/4 7/4])) 4) ; 35/16
(/ (reduce + (map square [1/4 1/2 3/4 1 5/4 6/4 7/4 1])) 4) ; 39/16

;; It's clear that these values will get closer to one another the more points we sample at

;; If we continue the procedure indefinitely:
(defn riemann-lower-sum [f a b N]
  (let [points (range a b (/ N))]
    (/ (reduce + (map f points)) N)))

(map (partial riemann-lower-sum square 0 2) (iterate inc 1)) ; (1 7/4 55/27 35/16 57/25 253/108 ...)
;; We find that the answers settle down, in this case to 8/3

;; Strictly speaking, the riemann lower sum is a sum of the lowest values of the function

(take 10 (map (partial - 8/3) (map (partial riemann-sum square 0 2) (iterate (partial * 2) 1)))) ; (5/3 11/12 23/48 47/192 95/768 191/3072 383/12288 767/49152 1535/196608 3071/786432)

;; If this procedure has a limit, then the integral is defined as this limit.
;; For well behaved functions, this limit will exist.

;; The limit can be thought of as the area under the graph of f from a to b.

;; At school we learn to calculate this limit by finding an antiderivative to the original function
;; An antiderivative of (fn[x] (* x x)) is (fn[x] (* 1/3 x x x))

(let [anti (fn[x] (* 1/3 x x x))]
  (- (anti 2) (anti 0))) ; 8/3

;; Although this is a very nice thing to be able to do, antiderivatives are
;; often very hard to find, and often it's just not possible, so we usually find
;; ourselves making numerical approximations.

;; A better estimate of this area is to imagine a trapezium with its base at (a,0) and (b,0), and the other corners at (a,f(a)) (b, f(b))

(defn trapezium [a fa b fb]
  (* 1/2 (- b a) (+ fa fb)))

;; So another first approximation to the integral is

(defn trapezium-rule [f a b]
  (trapezium a (f a) b (f b)))

(trapezium-rule square 0 2) ; 4

;; We can make another approximation by using the trapezium rule on
;; the two intervals [0,1] and [1,2] and adding the results

(+ (trapezium-rule square 0 1)
   (trapezium-rule square 1 2)) ; 3

;; And so on...

(defn iterated-rule [rule f a b N]
  (if (= N 0)
    (rule f a b)
    (let [midpoint (+ a (/ (- b a) 2))]
      (+ (iterated-rule rule f a midpoint (dec N))
         (iterated-rule rule f midpoint b (dec N))))))

;; This converges fairly nicely:
(map (partial iterated-rule trapezium-rule square 0 2) (range 10))
;; (4 3 11/4 43/16 171/64 683/256 2731/1024 10923/4096 43691/16384 174763/65536)

;; But an even nicer approximation is Simpson's rule, which involves fitting a parabola
;; to the two end points and the midpoint, and calculating the area under that.

(defn simpson-rule [f a b]
  (let [midpoint (+ a (/ (- b a) 2))]
    (* 1/6 (- b a) (+ (f a) (* 4 (f midpoint)) (f b)))))

;; For the square function, which is itself a parabola, that calculates the area exactly!
(simpson-rule square 0 2) ; 8/3

;; Let's try some harder functions

(defn sine [x] (Math/sin x))

;; here are the first few approximations with the trapezium rule:
(take 10 (map (partial iterated-rule trapezium-rule sine 0 Math/PI) (iterate inc 0))) ; (1.9236706937217898E-16 1.5707963267948968 1.8961188979370398 1.9742316019455508 1.993570343772339 1.9983933609701445 1.9995983886400377 1.9998996001842024 1.9999749002350526 1.9999937250705755)
;; After an inauspicious start, the answer is clearly settling down to 2, which is luckily the exact answer!

;; Simpson's rule also settles down to 2, but very quickly
(take 10 (map (partial iterated-rule simpson-rule sine 0 Math/PI) (iterate inc 0))) ; (2.094395102393196 2.0045597549844216 2.000269169948388 2.0000165910479364 2.000001033369414 2.0000000645300027 2.000000004032258 2.000000000252003 2.000000000015751 2.000000000000985)

;; What about if we're integrating a badly behaved function?:

(defn inverse [x] (/ x))

;; the real answer is log a - log b

(- (Math/log 1) (Math/log 0.0001)) ; 6.907755278982137

(take 10 (map (partial iterated-rule trapezium-rule inverse 0.0001 1) (iterate inc 0))) ; (4999.99995 2500.9997750199977 1251.8327765203333 627.5916420975113 315.8156999790102 160.27209317742054 82.84288624590312 44.46707207824598 25.61044440290146 16.499072595032356)

(take 10 (map (partial iterated-rule simpson-rule inverse 0.0001 1) (iterate inc 0))) ; (1667.9997166933313 835.4437770204453 419.5112639565708 211.89038593950997 108.42422424355732 57.03315060206397 31.67513402236028 19.324901844453297 13.461948659075995 10.812578055293251)

;; It looks like both the rules are converging to something, but the convergence is very slow.

;; Another function that doesn't do so well is:

(defn step [x]  (if (< x 1/2) 0 1))

(take 10 (map (partial iterated-rule trapezium-rule step 0.0001 1) (iterate inc 0))) ; (0.49995 0.749925 0.6249375 0.56244375 0.531196875 0.5155734375000001 0.50776171875 0.503855859375 0.5019029296874999 0.50092646484375)
(take 10 (map (partial iterated-rule simpson-rule step 0.0001 1) (iterate inc 0))) ; (0.8332500000000003 0.5832750000000001 0.5416125000000002 0.5207812500000001 0.5103656250000002 0.5051578125000001 0.5025539062500001 0.5012519531250001 0.5006009765625001 0.5002754882812501)

(defn evil-step [x]  (if (< x 1/3) 0 1))

(take 10 (map (partial iterated-rule trapezium-rule evil-step 0.0001 1) (iterate inc 0))) ; (0.49995 0.749925 0.6249375 0.68743125 0.656184375 0.6718078125 0.66399609375 0.667901953125 0.6659490234375001 0.66692548828125)
(take 10 (map (partial iterated-rule simpson-rule evil-step 0.0001 1) (iterate inc 0))) ; (0.8332500000000003 0.5832750000000001 0.7082625000000002 0.6457687500000002 0.6770156250000002 0.6613921875000002 0.6692039062500001 0.6652980468750002 0.6672509765625001 0.6662745117187502)
































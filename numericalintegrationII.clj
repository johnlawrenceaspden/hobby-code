;; Numerical Integration: Better Approximations

;; Remember our example function:

(defn square[x] (* x x))

;; We're trying to find ways to calculate the 'area under its graph between 0
;; and 2', or the 'integral over the interval [0,2]'

;; A better estimate of this area under the graph is to imagine a trapezium
;; which has its base corners at (0,0) and (2,0), and the top corners at
;; (0,(square 0)) (2, (square 2)), and calculate the area of that.

;; More generally, if the interval we're interested is [a,b], and the function's
;; values there are fa and fb, then the area of the trapezium will just be:

(defn trapezium [a fa b fb]
  (* 1/2 (- b a) (+ fa fb)))

;; Another way to think about that is that it's the length of the interval
;; multiplied by the average of the values of the function at the ends.

;; So another approximation to the integral of f over [a,b] is:

(defn trapezium-rule [f a b]
  (trapezium a (f a) b (f b)))

(trapezium-rule square 0 2) ; 4

;; We can make another approximation by using the trapezium rule on the two
;; subintervals [0,1] and [1,2] and adding the results

(+ (trapezium-rule square 0 1)
   (trapezium-rule square 1 2)) ; 3

;; And an even better one by splitting those subintervals in half

(+ (trapezium-rule square 0 1/2)
   (trapezium-rule square 1/2 1)
   (trapezium-rule square 1 3/2)
   (trapezium-rule square 3/2 2)) ; 11/4

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

(map (partial - 8/3)
     (map (partial iterated-rule trapezium-rule square 0 2) (range 10)))
;; (-4/3 -1/3 -1/12 -1/48 -1/192 -1/768 -1/3072 -1/12288 -1/49152 -1/196608)

;; We now only need a thousand samples of the function to get the answer
;; accurate to one part in 100000.

;; But an even nicer approximation is Simpson's rule, which involves fitting a
;; parabola to the two end points and the midpoint, and calculating the area
;; under that.

;; That's equivalent to taking a weighted sum of the values of f at the
;; beginning, midpoint, and end of the interval, with weights 1:4:1

(defn simpson-rule [f a b]
  (let [midpoint (+ a (/ (- b a) 2))]
    (* 1/6 (- b a) (+ (f a) (* 4 (f midpoint)) (f b)))))

;; For the square function, which is itself a parabola, this rule actually
;; calculates the area exactly!
(simpson-rule square 0 2) ; 8/3

;; That about wraps it up for numerical approximations to the integrals of
;; quadratics, but they are easy to calculate exactly anyway.






























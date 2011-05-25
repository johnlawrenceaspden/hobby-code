;; Numerical Integration I: What is an integral, anyway?

;; What does it mean to 'evaluate the integral' of a function over an interval?

;; Let's take an example function

(defn square[x] (* x x))

;; We could evaluate it at a few input values:
(map square [0 1 2]) ; (0 1 4)

;; The integral of a function over an interval is a sort of
;; 'average sum over all the possible values in the interval'.

;; A first approximation to the integral of square over the interval [0,2]
;; would be to say:

;; Let's pretend that the value of square between 0 and 1 is like (square 0)
;; and the value between 1 and 2 is like (square 1)

;; So to find our approximation we add (square 0) to (square 1):

(reduce + (map square [0 1])) ; 1

;; Another approximation, equally valid, would be to say that the value between
;; 0 and 1 is like (square 1) so we could equally well add the value at 1 to the
;; value at 2

(reduce + (map square [1 2])) ; 5

;; These answers are quite different! One is too low, because we paid too much
;; attention to the numbers at the start of the sub-ranges. One is too high,
;; because we added up numbers from the ends.

;; We can make both answers more accurate by sampling the function at more
;; points, and dividing the sum by an appropriate factor to make up for having
;; more points.

(/ (reduce + (map square [0 1/2 1 3/2]  )) 2) ; 7/4
(/ (reduce + (map square   [1/2 1 3/2 2])) 2) ; 15/4

;; We can continue the procedure, sampling the function at twice as many points again:

(/ (reduce + (map square [0 1/4 1/2 3/4 1 5/4 6/4 7/4]  )) 4) ; 35/16
(/ (reduce + (map square   [1/4 1/2 3/4 1 5/4 6/4 7/4 2])) 4) ; 51/16

;; It's clear that these values will get closer to one another the more points
;; we sample at

;; If we continue the procedure indefinitely:
(defn riemann-lower-sum [f a b N]
  (let [points (range a b (/ N))]
    (/ (reduce + (map f points)) N)))

(map (partial riemann-lower-sum square 0 2) (iterate inc 1)) ; (1 7/4 55/27 35/16 57/25 253/108 ...)
;; We find that the answers settle down to a particular number, and in this case
;; it looks like they're settling down to 8/3

;; However we do need to take quite a lot of samples to make the approximation get close!
(take 15 (map (partial - 8/3)
              (map (partial riemann-lower-sum square 0 2)
                   (iterate (partial * 2) 1))))
;; (5/3 11/12 23/48 47/192 95/768 191/3072 383/12288 767/49152 1535/196608
;; 3071/786432 6143/3145728 12287/12582912 24575/50331648 49151/201326592
;; 98303/805306368)

;; The last number here is what we get if we keep splitting our sub-intervals in
;; half fifteen times, and that's quite a lot of sub-intervals. (1, 2, 4, 8, 16 ...)

;; Strictly speaking, the riemann lower sum is a sum of the lowest values of
;; the function on the subintervals, but because the square function is
;; monotonically increasing over our range, the function as we defined it has
;; the same value.

;; If this procedure has a limit, then the integral is defined as this limit.
;; For well behaved functions, this limit will exist.

;; The limit can be thought of as the area under the graph of f from a to b.

;; At school we learn to calculate this limit by finding an antiderivative to
;; the original function, and calculating how much it changes over the interval.

;; An antiderivative of (fn[x] (* x x)) is (fn[x] (* 1/3 x x x))

(let [anti (fn[x] (* 1/3 x x x))]
  (- (anti 2) (anti 0))) ; 8/3

;; Although this is clearly a much better way to calculate integrals than by
;; taking millions of function samples and adding them up, it's often quite hard
;; to find antiderivatives even for quite simple functions, and for some quite
;; simple functions, no easily expressible antiderivative exists.

;; So we quite often find ourselves needing to calculate approximations by computer,
;; and this is called numerical integration.


























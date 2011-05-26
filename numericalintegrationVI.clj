;; Numerical Integration: Better Refinements?


;; Here are some very simple functions which we might want to test integration
;; methods on:
(defn square  [x] (* x x))
(defn sine    [x] (Math/sin x))
(defn step    [x] (if (< x 1/2) 0.0 1.0))
(defn inverse [x] (/ x))

;; Here are some Newton-Cotes formulae for approximate integration:

(defn trapezium-rule [f a b]
  (* 1/2 (- b a) (+ (f a) (f b))))

(defn simpson-rule [f a b]
  (let [midpoint (+ a (/ (- b a) 2))]
    (* 1/6 (- b a) (+ (f a) (* 4 (f midpoint)) (f b)))))

(defn simpson38-rule [f a b]
  (let [midpoint1 (/ (+ a a b) 3)
        midpoint2 (/ (+ a b b) 3)]
    (* 1/8 (- b a) (+ (f a) (* 3 (f midpoint1)) (* 3 (f midpoint2)) (f b)))))

(defn booles-rule [f a b]
  (let [midpoint1 (/ (+ a a a b) 4)
        midpoint2 (/ (+ a a b b) 4)
        midpoint3 (/ (+ a b b b) 4)]
    (* 1/90 (- b a) (+ (* 7 (f a)) (* 32 (f midpoint1)) (* 12 (f midpoint2)) (* 32 (f midpoint3)) (* 7 (f b))))))

;; And here is a way to apply them to (power 2 N) subintervals
(defn iterated-rule [rule f a b N]
  (if (= N 0)
    (rule f a b)
    (let [midpoint (+ a (/ (- b a) 2))]
      (+ (iterated-rule rule f a midpoint (dec N))
         (iterated-rule rule f midpoint b (dec N))))))

;; Here's a method of applying refinement where it is needed
(defn adaptive-rule-recurse [rule f a b desired-error]
  (let [guess (rule f a b)
        midpoint (/ (+ a b) 2)
        better-guess (+ (rule f a midpoint) (rule f midpoint b))
        error-estimate (- guess better-guess)
        abs-error-estimate (if (> error-estimate 0) error-estimate (- error-estimate))]
    (if (< abs-error-estimate desired-error) better-guess
        (let [half-desired-error (/ desired-error 2)]
          (+ (adaptive-rule-recurse rule f a midpoint half-desired-error)
             (adaptive-rule-recurse rule f midpoint b half-desired-error))))))

;; Let's make a function which gives us an integral and the estimated error on an interval

(defn approx-with-error[rule f a b]
  (let [guess (rule f a b)
        midpoint (/ (+ a b) 2)
        better-guess (+ (rule f a midpoint) (rule f midpoint b))
        error-estimate (- guess better-guess)
        abs-error-estimate (if (> error-estimate 0) error-estimate (- error-estimate))]
    [abs-error-estimate better-guess a b]))


(approx-with-error booles-rule inverse 0.1 10) ; [3.229853355941434 6.6432971603133 0.1 10]

(approx-with-error booles-rule inverse 0.1 5.5 ) ; [1.5313932520706137 4.788901774880001 0.1 5.5]
(approx-with-error booles-rule inverse 5.5 10 ) ; [9.61053111303567E-6 0.5978372320470694 5.5 10]

;; Now a function which takes a list of such things, and splits the interval with the largest error
(sort (list 
 (approx-with-error booles-rule inverse 0.1 5.5 ) ; [1.5313932520706137 4.788901774880001 0.1 5.5]
 (approx-with-error booles-rule inverse 5.5 10 ) ; [9.61053111303567E-6 0.5978372320470694 5.5 10]
 ))

(defn improve[rule f interval-list]
  (let [sorted (reverse (sort interval-list))
        [err val a b] (first sorted)
        remains (rest sorted)
        midpoint (/ (+ a b) 2)
        aa (approx-with-error rule f a midpoint)
        bb (approx-with-error rule f midpoint b)]
    (cons aa (cons bb remains))))


(defn improve-loop [rule f interval-list count]
  (loop [il interval-list count count]
    (if (zero? count) il
        (recur (improve rule f il) (dec count)))))

(-(Math/log 10000)(Math/log 0.00001)) ; 20.72326583694641

(reduce + (map second (improve-loop booles-rule inverse [[0 0 0.00001 10000]] 100))) ; 20.723265861971417

(reduce + (map second (improve-loop booles-rule inverse [[0 0 0.00001 10000]] 1000))) ; 20.723265836946407


(defn evil-improve-loop [rule f a b count]
  (let [pq (java.util.PriorityQueue. count (comparator (fn[a b](> (first a)(first b)))))]
    (.add pq (approx-with-error rule f a b))
    (loop [pq pq count count]
      (if (zero? count) pq
          (let [[err val a b] (.poll pq)
                midpoint (/ (+ a b) 2)
                aa (approx-with-error rule f a midpoint)
                bb (approx-with-error rule f midpoint b)]
            (doto pq
              (.add aa)
              (.add bb))
            (recur pq (dec count)))))))


(defn go[n]
  (let [things (evil-improve-loop booles-rule inverse 0.00000001 10000000 n)]
    [(reduce + (map second things))
     (reduce + (map first things))]))

(-(Math/log 10000000)(Math/log 0.00000001)) ; 34.538776394910684
(go 1) ; [1.9444444444448047E13 1.944444444444373E13]
(go 10) ; [3.797743056542089E10 3.797743055486256E10]
(go 100) ; [34.53877704296225 3.3430724324184924E-5]
(go 1000) ; [34.53877639491147 4.549938203979309E-11]
(go 10000) ; [34.53877639491065 9.361001557239845E-16]

(defn integrate[ f a b n ]
  (let [things (evil-improve-loop booles-rule f a b n)]
    [(reduce + (map second things))
     (reduce + (map first things))]))


(integrate square 0 2 10) ; [8/3 0]
(integrate sine 0 Math/PI 10) ; [1.9999999999725113 1.7383848804897184E-9]
(integrate sine 0 Math/PI 30) ; [1.9999999999999762 1.463142107471782E-12]
(integrate sine 0 Math/PI 100) ; [1.9999999999999991 3.1931922384043077E-15]
(integrate sine 0 Math/PI 1000) ; [1.999999999999999 2.526233413538588E-17]

(integrate inverse 0.0001 2 10) ; [10.004656962088271 0.33632812781504795]
(integrate inverse 0.0001 2 20) ; [9.903491702279531 1.5952362483767635E-4]
(integrate inverse 0.0001 2 1000) ; [9.903487552536125 7.435024818036595E-15]


    


















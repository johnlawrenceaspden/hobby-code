;; One of the most beautiful things I learned in the first year at college was
;; the idea that there are angles between functions. This is common knowledge
;; amongst pure mathematicians.

;; Essentially, if you understand the dot product of vectors, then you
;; understand what Fourier Analysis is already.

;; I've only ever been able to explain this idea to one or two people, since
;; it's quite abstract, so even people who use Fourier techniques day to day
;; have trouble getting it.

;; But it occurs to me that if I can explain it to a computer, and get the
;; computer to draw pictures, then I might have more success with it, and it
;; deserves to be more widely known.

;; So let's first of all program the dot product that everyone's familiar with:

(ns school-vectors
  (:refer-clojure :exclude [resolve])
  (:use simple-plotter))

;; (x1, x2, x3) . (y1, y2, y3) = x1.y1+x2.y2+x3.y3

;; In clojure, we might say:

(defn make-vector [& a] (apply vector a))

;; And here are the three usual basis vectors of three-dimensional space, at right angles to each other.
(def e1 (make-vector 1 0 0))
(def e2 (make-vector 0 1 0))
(def e3 (make-vector 0 0 1))
;; e1 [1 0 0] e2 [0 1 0] e3 [0 0 1]

;; Now how do we calculate the dot product of two vectors?

(defn dot-product [v1 v2]
  (reduce + (map * v1 v2)))

(dot-product e1 e1) ;1
(dot-product e1 e2) ;0

;; What about more general vectors?
(def u (make-vector 1 2 3)) ; u [1 2 3]
(def v (make-vector 3 4 5)) ; v [3 4 5]

(dot-product u v) ; 26 = 1*3+2*4+3*5

;; And notice that the dot product of a vector with one of the basis vectors
;; just gives us its component in that direction:

(dot-product u e1) ;1
(dot-product u e2) ;2
(dot-product u e3) ;3

;; We can add vectors
(defn sum [v1 v2]
  (apply make-vector (map + v1 v2)))

(sum u v); [4 6 8]

;; And we can multiply them by constants
(defn product [ a v ]
  (apply make-vector (map #(* a %) v)))

(product 10 v) ; [30 40 50]

;; We can calculate the lengths of vectors by Pythagoras' Theorem
;; length^2 = x^2 + y^2 + z^2

(defn length [v]
  (Math/sqrt (dot-product v v)))

(length u) ; 3.7416573867739413
(length v) ; 7.0710678118654755


;; Notice that all the basis vectors are length 1
(map length (list e1 e2 e3)) ; (1.0 1.0 1.0)

;; If we have a vector, then we can make a unit vector in its direction by dividing it by its length
(defn unit-vector [v]
  (product (/ 1 (length v)) v))

(unit-vector v) ;; [0.4242640687119285 0.565685424949238 0.7071067811865475]

;; Of course this will have length 1 (as far as floating point arithmetic can
;; tell us)
(length (unit-vector v)) ;;0.9999999999999999 sigh... 

;; So one thing the dot-product gives us is a notion of the length of things,
;; but it also gives us a notion of the angles between them, independently of
;; actually drawing them and measuring the angle with a protractor.

;; The dot product of u and v, as every schoolboy knows, is the length of
;; u times the length of v times the cosine of the angle between them.

;; And that means that we can work out the cosine of the angle between the two
;; vectors just using the dot-product:
(defn cos-angle [ v1 v2 ]
  (/ (dot-product v1 v2) (length v1) (length v2)))

;; So we can work out the angle between any two vectors (that's
;; what acos is for)
(defn angle [v1 v2] (Math/acos (cos-angle v1 v2))) 

(angle u v) ; 0.18623876586485016

;; Thats in radians of course. So if your circle is 1 meters across, you need to
;; go 18.62.. cms around an arc to make that angle.  that's about 1/30th of all
;; the way round the circle, or 1/30th of a full turn

(def tau (* 2.0 Math/PI)) ;; tau is the number of radians in a full turn

(defn radians-to-turns[r]
  (/ r tau))

(defn turns-between [u v]
  (radians-to-turns (angle u v)))

(turns-between u v) ; 0.02964082018272505, or about 3%, of a full turn.
(turns-between u u) ; 0.0 the angle between a vector and itself is always 0
(turns-between v v) ; 2.371593461809983E-9 , or close to zero if you've got dodgy floating point.

;; what is the angle between the basis vectors?

(turns-between e1 e1) ;0.0
(turns-between e1 e2) ;0.25 one quarter of a turn. A right angle.

(partition 3
           (for [x (list e1 e2 e3)
                 y (list e1 e2 e3)]
             (turns-between x y)))
;; Distinct vectors in the usual basis are at right angles to one another.


;; So far, this is school maths that everyone should dimly remember.  Let's draw
;; a picture, just forgetting about the z coordinate and drawing x and y:
(use 'simple-plotter)

(defn projection-plot [v]
  (apply line 0 0 (take 2 v)))

(def projection-window (create-window "U and V, 3% of a turn apart" 200 200 white black -0.1 5 -0.1 5))
(ink red)
(projection-plot u)
(ink blue)
(projection-plot v)
;; Let's see the unit vectors of u and v
(ink green)
(projection-plot (unit-vector u))
(projection-plot (unit-vector v))
;; Now let's add the basis vectors for comparison
(ink yellow)
(doall (map projection-plot (list e1 e2 e3)))

;; Notice that e1 and e2 look like unit vectors, but e3 has disappeared, because
;; we're looking at it head on, and the unit vectors of u and v don't look long
;; enough, because they're at an angle to our diagram, so they get foreshortened.

;; There's also the idea of resolving one vector against another
(defn resolve [u v]
  (product (/ (dot-product u v) (length v)) (unit-vector v)))

;; This gives us a vector in the direction of v that is as close to u as you can
;; get.
(ink magenta)
(projection-plot (resolve u v))
;; And a vector in the direction of u that is a close as such a thing can be to v
(projection-plot (resolve v u))

;; One problem with this sort of picture is that we lose information. We've thrown away the z coordinate by drawing it. In the case of e3, the z coordinate was all there was, so it doesn't look very long in the diagram.

;; What if we just plot the three values?

(defn coords-plot [v]
  (let [coords-seq (partition 2 (interleave (range) v))]
    (apply plot (first coords-seq))
    (doseq [[x y] (rest coords-seq)] (draw-to x y))))
        

(create-window "U and V's coordinates" 200 200 white black 0 2 0 5)
(ink red)
(coords-plot u)
(ink blue)
(coords-plot v)
(ink yellow)
(doall (map coords-plot (list e1 e2 e3)))
(ink green)
(coords-plot (unit-vector u))
(coords-plot (unit-vector v))

;; We can see the vectors better this way, because we can see all their
;; coordinates at once, even though we can no longer see the space they're in!

;; We can build the vectors up, bit by bit, out of basis vectors, by resolving
;; them against the coordinate vectors.
(ink magenta)
(coords-plot (resolve u v))
;; In this view, (resolve u v) is a scaled version of v that is as close as it can be to being u
;; Similarly:
(coords-plot (resolve v u))
;; Is a scaled-up version of v that's as close as it can be to u

;; This gives us the idea of building up vectors bit by bit
(create-window "Making U up out of sums of bits" 200 200 white black 0 2 0 5)

(def u1 (resolve u e1))
(def u2 (resolve u e2))
(def u3 (resolve u e3))
(def u12 (sum u1 u2))
(def u123 (sum u12 u3))
(ink green)
(coords-plot u1)
(coords-plot u2)
(coords-plot u3)
(ink blue)
(coords-plot u12)
(ink red)
(coords-plot u123)



;; We can also look at that on our projection plot (where we don't see the third coordinate, remember.
(create-window "Making U out of bits" 200 200 white black -0.1 5 -0.1 5)
(ink green)
(projection-plot u1)
(projection-plot u2)
(projection-plot u3)
(ink blue)
(projection-plot u12)
(ink red)
(projection-plot u123)

(ink red)
(let [u1 (resolve u e1)
      u12 (sum u1 (resolve u e2))
      u123 (sum u12 (resolve u e3))]
  (projection-plot u1)
  (projection-plot u12)
  (projection-plot u123))

;; So u =
(reduce sum (list (resolve u e1) (resolve u e2) (resolve u e3))) ; [1.0 2.0 3.0]


;; Now, although e1, e2, e3, are made special by our coordinate system, there is
;; not actually much that is special about them except for that. Any three unit
;; vectors at right angles to each other will do as a basis for resolving against.

;; The important thing about a basis is that the dot-product of any vector with
;; itself is 1 and with any other member of the basis is 0
(partition 3 (for [x (list e1 e2 e3)
                   y (list e1 e2 e3)]
               (dot-product x y)))

;; or equivalently that the angles between distinct basis vectors are right angles (quarter turns)
;; and that they are all length one

(map length (list e1 e2 e3))
(partition 3 (for [x (list e1 e2 e3)
                   y (list e1 e2 e3)]
               (turns-between x y)))

;; Such a set is called a basis.

;; We could try some other vectors as our basis
(def f1 (unit-vector (make-vector  1  1  0)))
(def f2 (unit-vector (make-vector  1 -1  0)))
(def f3 (unit-vector (make-vector  0  0  1)))

;;move along... nothing to see here...
(defn ignore-float-noise[x] (/ (Math/round (* x 100000000.0)) 100000000.0))

(defn basis-test [& f]
  (partition (count (first f))
             (map #(ignore-float-noise %)
                  (for [x f
                        y f]
                    (dot-product x y)))))

(basis-test f1 f2 f3)

;; Here's another possible set
(def g1 (unit-vector (make-vector  1 1 1)))  ; [0.5773502691896258 0.5773502691896258 0.5773502691896258]
(def g2 (unit-vector (make-vector -1 0 1)))  ; [-0.7071067811865475 0.0 0.7071067811865475]
(def g3 (unit-vector (make-vector -1 2 -1))) ; [-0.4082482904638631 0.8164965809277261 -0.4082482904638631]

(basis-test g1 g2 g3)


;; Let's plot the various sets of vectors
(create-window "Basis vector sets" 200 200 white black -1 1 -1 1)
(ink yellow)
(doseq [x (list e1 e2 e3)] (projection-plot x))
(ink green)
(doseq [x (list f1 f2 f3)] (projection-plot x))
(ink red)
(doseq [x (list g1 g2 g3)] (projection-plot x))

;; And now as coordinate plots
(create-window "Basis vector sets" 200 200 white black 0 2 -1 1)
(ink yellow)
(doseq [x (list e1 e2 e3)] (coords-plot x))
(ink green)
(doseq [x (list f1 f2 f3)] (coords-plot x))
(ink red)
(doseq [x (list g1 g2 g3)] (coords-plot x))


;; Just check, at this point, that you understand all this so far. It is just
;; simple vector geometry, of the type that is generally taught to fifteen-year
;; olds in school maths class.

;; Here's the first thing that might not be new:

;; We can also take advantage of the way that sin x, and cos x and the constant
;; function oscillate out of phase with each other, as long as we sample at
;; points equally spaced around the unit circle

(def angle-list (list 0 (* tau 1/3) (* tau 2/3)))

(def g1 (unit-vector (map (constantly 1) angle-list)))
(def g2 (unit-vector (map #(Math/sin %) angle-list)))
(def g3 (unit-vector (map #(Math/cos %) angle-list)))


;; Don't worry if that doesn't make sense!
;; The important thing is that sin, cos and constant give us a set of vectors at right angles to each other.

(basis-test g1 g2 g3)

;; If you look closely, you'll see that they're actually just one of the sets we guessed before.

;; Because they're a basis, they're equally good for splitting vectors into bits
;; (with resolve) and putting them back together (with sum)
(reduce sum (list (resolve u g1) (resolve u g2) (resolve u g3))) ;;[1.0000000000000009 1.9999999999999998 3.0]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; But nowhere so far have we said that vectors have to have only three numbers in them!!

;; Exactly the same functions work for vectors of five numbers, and although the
;; projection plot is going to be pretty useless here, the coordinate plot still
;; works fine.

(ns five-vectors
  (:refer-clojure :exclude [resolve])
  (:use simple-plotter)
  (refer school-vectors :only [unit-vector length coords-plot radians-to-turns resolve basis-test
                               dot-product product make-vector turns-between angle sum tau]))

(def five-vector-window (create-window "Five element vectors" 200 200 white black 0 4 0 10))

;; Let's make new u and v with five components
(def u [ 2 4 7 8 3 ])
(def v [ 7 6 5 9 8 ])

(ink red)
(coords-plot u)
(ink blue)
(coords-plot v)

;; Our definition of angle still works
(turns-between u v) ;; 0.07600513530526805
;; u and v have 7.6% of a full turn between them (about 28 degrees)

;; We are doing geometry in a five-dimensional space! Our intuitions and calculation techniques still work!

;; Our basis vectors (we need five of them now) are:
(def e1 [1 0 0 0 0])
(def e2 [0 1 0 0 0])
(def e3 [0 0 1 0 0])
(def e4 [0 0 0 1 0])
(def e5 [0 0 0 0 1])

;; The five basis vectors still look roughly the same:
(ink green)
(doseq [e (list e1 e2 e3 e4 e5)] (coords-plot e))

;; The important thing about a basis is still that the dot-product of any vector
;; with itself is 1 and with any other member of the basis is 0
(partition 5 (for [x (list e1 e2 e3 e4 e5)
                   y (list e1 e2 e3 e4 e5)]
               (dot-product x y)))

(basis-test e1 e2 e3 e4 e5)

;; And it's still true that:

(reduce sum (map #(resolve u %) (list e1 e2 e3 e4 e5))) ;[2.0 4.0 7.0 8.0 3.0] = u

;; We can look at all the individual resolved bits
(def bits-of-u (map #(resolve u %) (list e1 e2 e3 e4 e5)))

(doseq [x bits-of-u] (coords-plot x))

;; And we can look at how adding those bits up makes the whole
(ink magenta)
(doseq [x (reductions sum bits-of-u)] (coords-plot x))


;; And our way of getting the sin and cos and constant functions to make a basis for us also generalizes
(def angle-list (list 0 (* tau 1/5) (* tau 2/5) (* tau 3/5) (* tau 4/5)))

(def g1 (unit-vector (map (constantly 1) angle-list)))
(def g2 (unit-vector (map #(Math/sin %) angle-list)))
(def g3 (unit-vector (map #(Math/cos %) angle-list)))
(def g4 (unit-vector (map #(Math/sin (* 2 %)) angle-list)))
(def g5 (unit-vector (map #(Math/cos (* 2 %)) angle-list)))

(basis-test g1 g2 g3 g4 g5)

(do
  (create-window "sin and cos basis" 200 200 white black 0 4 -1 1)
  (ink white)
  (coords-plot g1)
  (ink green)
  (coords-plot g2)
  (ink blue)
  (coords-plot g3)
  (ink yellow)
  (coords-plot g4)
  (ink magenta)
  (coords-plot g5))

(def more-bits-of-u (map #(resolve u %) (list g1 g2 g3 g4 g5)))

(do
  (create-window "Resolving onto sin and cos basis" 200 200 white black 0 4 -10 10)
  (ink red)
  (coords-plot u)
  (ink white)
  (coords-plot (nth more-bits-of-u 0))
  (ink green)
  (coords-plot (nth more-bits-of-u 1))
  (ink blue)
  (coords-plot (nth more-bits-of-u 2))
  (ink yellow)
  (coords-plot (nth more-bits-of-u 3))
  (ink magenta)
  (coords-plot (nth more-bits-of-u 4)))


;; I'm getting bored of writing this out

(defn plot-sequence [vectors]
  (user/def-let [colors (cycle (map #(java.awt.Color. (* 40 %) 120 0) (range 5))) #_(cycle (list white green blue yellow magenta pink cyan orange red lightgray gray darkgray))
        cv (partition 2 (interleave colors vectors))]
    (doseq [[c v] cv]
      (ink c)
      (coords-plot v))))


(create-window "Successive approximations" 200 200 white black 0 4 0 10)
(ink red)
(coords-plot u)
(plot-sequence (reductions sum more-bits-of-u))



(def red )


























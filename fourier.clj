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

;; (x1, x2, x3) . (y1, y2, y3) = x1.y1+x2.y2+x3.y3

;; In clojure, we might say:

(defn make-vector [& a] (apply vector a))

;; And here are the three usual basis vectors, at right angles to each other.
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
;; just gives us the component in that direction:

(dot-product u e1) ;1
(dot-product u e2) ;2
(dot-product u e3) ;3

;; Now we can add vectors
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

;; And that the dot-product of any e? with any other e? is 0
(partition 3 (for [x (list e1 e2 e3)
                   y (list e1 e2 e3)]
               (dot-product x y)))

;; If we know the length of a vector, then we can make a unit vector in its direction by dividing it by its length

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
;; vectors:
(defn cos-angle [ v1 v2 ]
  (/ (dot-product v1 v2) (length v1) (length v2)))

;; And that means that we can work out the angle between the vectors (that's
;; what acos is for)
(defn angle [v1 v2] (Math/acos (cos-angle v1 v2))) 

(angle u v) ; 0.18623876586485016

;; Thats in radians of course. So if your circle is 1 meters across, you need to
;; go 18.62.. cms around an arc to make that angle.  that's about 1/30th of all
;; the way round the circle, or 1/30th of a full turn

(defn radians-to-turns[r]
  (/ r (* 2.0 Math/PI)))

(defn turns-between [u v]
  (radians-to-turns (angle u v)))

(turns-between u v) ; 0.02964082018272505, or about 3%, of a full turn.

;; So far, this is school maths that everyone should dimly remember.  Let's draw
;; a picture, just forgetting about the z coordinate and drawing x and y:
(use 'simple-plotter)

(defn projection-plot [v]
  (apply line 0 0 (take 2 v)))

(create-window "U and V, 3% of a turn apart" 200 200 white black -0.1 5 -0.1 5)
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

;; Notice that e1 and e2 look like unit vectors, but e3 has disappeared, because we're looking at it head on, and the unit vectors of u and v don't look long enough, because they're at an angle to our diagram, so the get foreshortened.

;; There's also the idea of resolving one vector against another
(defn resolve [u v]
  (product (/ (dot-product u v) (length v)) (unit-vector v)))

;; This gives us a vector in the direction of v that is as close to u as you can
;; get.
(ink magenta)
(projection-plot (resolve u v))


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

;; We can see the vectors better this way, because we can see all their coordinates at once, even though we can no longer see the space they're in!

;; We can build the vectors up, bit by bit, out of basis vectors, by resolving
;; them against the coordinate vectors.
(ink magenta)
(coords-plot (resolve u v))
;; In this view, (resolve u v) is a scaled version of v that is as close as it can be to being u
;; Similarly:
(coords-plot (resolve v u))

;; This gives us the idea of building up vectors bit by bit
(let [u1 (resolve u e1)
      u12 (sum u1 (resolve u e2))
      u123 (sum u12 (resolve u e3))]
  (coords-plot u1)
  (coords-plot u12)
  (coords-plot u123))




































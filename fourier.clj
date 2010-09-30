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

;; We can calculate the lengths of vectors by Pythagoras' Theorem length^2 = x^2 + y^2 + z^2

(defn length [v]
  (Math/sqrt (dot-product v v)))

(length v) ; 7.0710678118654755
(length u) ; 3.7416573867739413

;; Notice that all the basis vectors are length 1
(map length (list e1 e2 e3)) ; (1.0 1.0 1.0)

;; Now, the cosine of the angle between two vectors is 
(defn cos-angle [ v1 v2 ]
  (/ (dot-product v1 v2) (length v1) (length v2)))

(defn angle [v1 v2] (Math/acos (cos-angle v1 v2)))

(angle u v) ; 0.18623876586485016

;; Thats in radians of course. So if your circle is 1 meters across, you need to go 19 cms around an arc to make that angle.
;; that's about 1/30th of all the way round the circle, or full turn

(defn radians-to-turns[r]
  (/ r (* 2.0 Math/PI)))

(defn turns-between [u v]
  (radians-to-turns (angle u v)))

(turns-between u v) ; 0.02964082018272505, or about 3%, of a full turn.

;; So far, this is school maths that everyone should dimly remember.
;; Let's draw a picture, just forgetting about the z coordinate and drawing x and y:
(use 'simple-plotter)

(defn projection-plot [v]
  (apply line 0 0 (take 2 v)))

(create-window "U and V, 3% of a turn apart" 200 200 white black -0.1 5 -0.1 5)
(ink red)
(projection-plot u)
(ink blue)
(projection-plot v)
;; Now let's add the basis vectors
(ink yellow)
(doall (map projection-plot (list e1 e2 e3)))

;; One problem with this sort of picture is that we lose information. We've thrown away the z coordinate by drawing it.

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

;; We can see the vectors better this way, even though we can no longer see the space they're in!







































;; My recursion tells me that n planes can divide space into
;; 2, 4, 8, 15, 26, and finally 42 regions.

;; But I'd like some sort of empirical confirmation of this result.

;; Both forms and vectors in 3 space can be represented as ordered sets of three numbers
(def a-form [1 2 3])

(def a-point [2 3 5])

;; And we evaluate the form on the vector
(defn contract [form vector]
  (reduce + (map * form vector)))
;; (or if you prefer, take the dot product, or contract the tensors)

(contract a-form a-point) ;-> 23

;; Any plane can be defined in terms of a 1-form and a number.  
(def a-plane {:form [1 2 3] :val 4})

;; Now if we have a plane or a vector, we can evaluate the form on the
;; vector, and compare the result with the value. This tells us which
;; side of the plane the vector is on.

(defn side [plane point]
  (- (contract (plane :form) point) (plane :val)))


(side a-plane [2 3  5]) ;-> 19  (this point is on the positive side)
(side a-plane [2 3 -5]) ;-> -11 (on the negative side)
(side a-plane [2 3 -4/3]) ;-> 0N (in the plane itself)


;; Ok, now we need a way of taking vectors and forms at random. 
;; The cauchy distribution is easy to sample from and has nice fat tails

(defn cauchy[]
  (Math/tan (* Math/PI (- (rand) 0.5))))

(repeatedly 20 cauchy) ;-> (-0.43989542100474244 -0.6517139433588255 1.58518947555566 0.001268073580101198 3.6164981498788262 0.44928717656825584 0.3365831420089349 0.4646894211443393 0.8802485518044282 1.8146747880005754 0.1608864471756546 -0.23538854021056904 8.836583912257565 3.8174659241864703 0.5387819323291936 -0.18830386363467239 -1.0430272980416788 0.3310448308016341 -0.10735190850334911 0.3426157380908667)

(defn make-point []
  (repeatedly 3 cauchy))

(defn make-plane []
  {:form (repeatedly 3 cauchy) :val (cauchy)})

(make-point) ;-> (33.032354006369815 -29.428219536044043 -37.796430533111334)
(make-plane) ;-> {:form (-45.36910184399889 -1.6741101969009575 9.952054197916382), :val 0.9505471630252558}

(def points (repeatedly #(make-point)))
(def planes (repeatedly #(make-plane)))

;; And we'll need a function to tell us the sign of a number
(defn sign[x]
  (if (< x 0) '- '+))

(map sign [ -1 -2 -3 0 -0.5 1.3]) ;-> (- - - + - +)

;; Now if we take a set of planes and a point, 
(defn sig [point planes]
  (for [p planes] (sign (side p point))))

(sig (first points) (take 3 planes)) ;-> (+ - +)

;; Every different region gives a different signature. 

(count (frequencies (take 10 (map #(sig % (take 1 planes)) points)))) ;-> 2
(count (frequencies (take 10 (map #(sig % (take 2 planes)) points)))) ;-> 4
(count (frequencies (take 10 (map #(sig % (take 3 planes)) points)))) ;-> 6
(count (frequencies (take 10 (map #(sig % (take 4 planes)) points)))) ;-> 7
(count (frequencies (take 10 (map #(sig % (take 5 planes)) points)))) ;-> 7
(count (frequencies (take 10 (map #(sig % (take 6 planes)) points)))) ;-> 7

;; The more points we take, the more likely we are to get one in every region

(count (frequencies (take 100 (map #(sig % (take 1 planes)) points)))) ;-> 2
(count (frequencies (take 100 (map #(sig % (take 2 planes)) points)))) ;-> 4
(count (frequencies (take 100 (map #(sig % (take 3 planes)) points)))) ;-> 7
(count (frequencies (take 100 (map #(sig % (take 4 planes)) points)))) ;-> 11
(count (frequencies (take 100 (map #(sig % (take 5 planes)) points)))) ;-> 18
(count (frequencies (take 100 (map #(sig % (take 6 planes)) points)))) ;-> 21

(count (frequencies (take 1000 (map #(sig % (take 1 planes)) points)))) ;-> 2
(count (frequencies (take 1000 (map #(sig % (take 2 planes)) points)))) ;-> 4
(count (frequencies (take 1000 (map #(sig % (take 3 planes)) points)))) ;-> 8
(count (frequencies (take 1000 (map #(sig % (take 4 planes)) points)))) ;-> 15
(count (frequencies (take 1000 (map #(sig % (take 5 planes)) points)))) ;-> 26
(count (frequencies (take 1000 (map #(sig % (take 6 planes)) points)))) ;-> 38

(count (frequencies (take 10000 (map #(sig % (take 1 planes)) points)))) ; 2 
(count (frequencies (take 10000 (map #(sig % (take 2 planes)) points)))) ; 4 
(count (frequencies (take 10000 (map #(sig % (take 3 planes)) points)))) ; 8 
(count (frequencies (take 10000 (map #(sig % (take 4 planes)) points)))) ; 15 
(count (frequencies (take 10000 (map #(sig % (take 5 planes)) points)))) ; 26 
(count (frequencies (take 10000 (map #(sig % (take 6 planes)) points)))) ; 40 

(count (frequencies (take 100000 (map #(sig % (take 1 planes)) points)))) ; 2 
(count (frequencies (take 100000 (map #(sig % (take 2 planes)) points)))) ; 4 
(count (frequencies (take 100000 (map #(sig % (take 3 planes)) points)))) ; 8 
(count (frequencies (take 100000 (map #(sig % (take 4 planes)) points)))) ; 15
(count (frequencies (take 100000 (map #(sig % (take 5 planes)) points)))) ; 26
(count (frequencies (take 100000 (map #(sig % (take 6 planes)) points)))) ; 41

(count (frequencies (take 1000000 (map #(sig % (take 1 planes)) points)))) ; 2
(count (frequencies (take 1000000 (map #(sig % (take 2 planes)) points)))) ; 4
(count (frequencies (take 1000000 (map #(sig % (take 3 planes)) points)))) ; 8
(count (frequencies (take 1000000 (map #(sig % (take 4 planes)) points)))) ; 15
(count (frequencies (take 1000000 (map #(sig % (take 5 planes)) points)))) ; 26
(count (frequencies (take 1000000 (map #(sig % (take 6 planes)) points)))) ; 42 

;; I'm painfully conscious of having stopped the experiment at the
;; exact point where I got the answer I expected. But my poor little
;; computer is not going to be up to running this for 10000000 points.

;; But this can't just be coincidence, surely?









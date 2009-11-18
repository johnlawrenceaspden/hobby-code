;;;; programs written whilst reading http://tihomir.org/crazy-questions-at-google-job-interview/

;;;;In a country where everyone wants boys, families have children until they get a boy. What is the sex ratio?
#|
(b)
(g b)
(g g b)
(g g g b)
(g g g g b)

if pop is the (very large) number of couples:

=> girls= pop/2 * 0 + pop/4 *1 + pop/8 * 2 + pop/16 * 3 + pop/32 * 4 ....
   boys = pop/2 + pop/4 + pop/8 + pop/16 = pop

girls = pop (1/4 + 2/8 + 3/16 + 4/32 + ....)

|#

(defun girlfrac (n)
  (if (= n 0) 0
      (+ (girlfrac (- n 1)) 
	 (/ n (expt 2 (+ n 1))))))

(mapcar #'girlfrac '(0 1 2 3 4 5 6))


;;;;If probability of seeing a car is 0.95 in 30 mins what is prob in 10?

(let ((p (- 1.0 (expt (- 1 0.95) 1/3))))
  (cube (- 1 p)))

(defun cube (n) (* n n n))

(cube (expt 0.05 1/3))

(defvar pnotseecar30min (- 1 0.95))
(defvar pnotseecar10min (expt pnotseecar30min 1/3))
(defvar pseecar10min (- 1 pnotseecar10min))

(/ 360 48)

(* 2 2 2 3 3)





6,4,3   13
6,6,2   14
8,3,3   14
9,4,2   15
12,3,2  17
9,8,1   18
12,6,1  19
18,2,2  22
18,4,1  23


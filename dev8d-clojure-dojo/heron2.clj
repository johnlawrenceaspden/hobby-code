;; Code from the Clojure Dojo at dev8d. Written collaboratively by
;; a room full of 30 people.


(defn avg[x y](/ (+ x y) 2))

(defn abs[x](if (> x 0) x (- x)))

(defn makeimpr[target]
        (fn[x]
          (avg x (/ target x))))

(defn makegoodenough [target epsilon]
        (fn[x]
          (< (abs (- target (* x x) )) epsilon)))

(defn iterativeimprove[x goodenough? improve]
        (if (goodenough? x)
          x
          (iterativeimprove (improve x) goodenough? improve)))

(defn sqrt [target] 
        (iterativeimprove target (makegoodenough target 1/1000000) (makeimpr target)))

(* 1.0 (sqrt 10) (sqrt 10))


;;Generalizing to Newton Raphson Solver

(defn f [x] (- (* x x x) 5))
(f 1.6)

(defn deriv[x]
  (/ (- (f (+ x 1/10000)) (f x)) 1/10000))

(deriv 1.6)

(f 1.74)

(deriv 1.74)

(defn makederiv[f]
  (fn[x]
    (/ (- (f (+ x 1/10000)) (f x)) 1/10000)))

((makederiv f) 1.60)  ; ((fn[x](* 3 x x)) 1.6)

(/(f 1.60)((makederiv f) 1.60))
(- 1.60 -0.11770097686909912)


(defn improver [f] 
  (fn [x]
    (- x (/ (f x) (deriv x))))) ;; bug here that no-one spotted. 

;;but it works for our particular problem.
((improver f) 1.6)
((improver f) 1.7177)
((improver f) 1.7100)
((improver f) 1.7099759484)
((improver f) 1.7099759484)
((improver f) 1.7099759466767976)
((improver f) 1.709975946676697 )
(f 1.709975946676697)

(defn makegoodenoughagain[f epsilon]
  (fn[x]
    (< (abs (f x )) epsilon)))

(defn solver[f] 
   (iterativeimprove 1.6 (makegoodenoughagain f 1/1000000) (improver f)))

(solver f)
(f (solver f))

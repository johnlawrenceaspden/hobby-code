(defn atom? [x] (not (instance? clojure.lang.IPersistentCollection x)))

(defn const? [expr val] (and (atom? expr) (not= expr val)))

(defn same-var? [expr val] (and (atom? expr) (= expr val)))

(defn arith? [expr sym] (and (not (atom? expr)) (= (first expr) sym)))

(defn sum? [expr] (arith? expr '+))

(defn product? [expr] (arith? expr '*))

(defn a1 [expr] (frest expr))
(defn a2 [expr] (first (rrest expr)))
(defn m1 [expr] (frest expr))
(defn m2 [expr] (first (rrest expr)))

(defn deriv [expr val]
  (cond
    (const? expr val) 0
    (same-var? expr val) 1
    (sum? expr)
       (list '+ (deriv (a1 expr) val) (deriv (a2 expr) val))
    (product? expr)
       (list '+ 
	     (list '* (m1 expr) (deriv (m2 expr) val))
	     (list '* (m2 expr) (deriv (m1 expr) val)))
    (:else) nil))


(deriv '(+ (* 1 2) (* x 3) (* x (* x x))) 'x)
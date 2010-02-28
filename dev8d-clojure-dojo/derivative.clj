;;Symbolic differentiation code from first clojure dojo at dev8d

(defn addition? [expr] 
  (and (list? expr) (= '+ (first expr)))
)

(defn multiple? [expr]
  (and (list? expr) (= '* (first expr)))
)

(defn make-addition [a b] 
  (list '+ a b)
)

(defn make-multiple [a b]
  (list '* a b)
)


(defn deriv [expr variable]
  (cond (= expr variable) 1
        (number? expr) 0
        (symbol? expr) 0
        (addition? expr) (make-addition 
                          (deriv (second expr) variable)
                          (deriv (third expr) variable) )
        (multiple? expr) (make-addition
                          (make-multiple ( second expr)
                                         (deriv (third expr) variable))
                          (make-multiple (deriv (second expr) variable )
                                         (third expr)))


        ))

(every? identity
        (list
         (= (deriv 1 'x) 0)
         (= (deriv 'x 'x) 1)
         (= (deriv 'y 'x) 0)
         (= (deriv '(+ x 1) 'x) '(+ 1 0))
         (= (deriv '(* x x) 'x) '(+ (* x 1) (* 1 x)))
         )  
        )
(deriv '(+ (* x x) 4) 'x)